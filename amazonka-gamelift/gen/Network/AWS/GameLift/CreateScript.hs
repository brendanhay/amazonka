{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new script record for your Realtime Servers script. Realtime
-- scripts are JavaScript that provide configuration settings and optional
-- custom game logic for your game. The script is deployed when you create
-- a Realtime Servers fleet to host your game sessions. Script logic is
-- executed during an active game session.
--
-- To create a new script record, specify a script name and provide the
-- script file(s). The script files and all dependencies must be zipped
-- into a single file. You can pull the zip file from either of these
-- locations:
--
-- -   A locally available directory. Use the /ZipFile/ parameter for this
--     option.
--
-- -   An Amazon Simple Storage Service (Amazon S3) bucket under your AWS
--     account. Use the /StorageLocation/ parameter for this option.
--     You\'ll need to have an Identity Access Management (IAM) role that
--     allows the Amazon GameLift service to access your S3 bucket.
--
-- If the call is successful, a new script record is created with a unique
-- script ID. If the script file is provided as a local file, the file is
-- uploaded to an Amazon GameLift-owned S3 bucket and the script record\'s
-- storage location reflects this location. If the script file is provided
-- as an S3 bucket, Amazon GameLift accesses the file at this storage
-- location as needed for deployment.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set Up a Role for Amazon GameLift Access>
--
-- __Related operations__
--
-- -   CreateScript
--
-- -   ListScripts
--
-- -   DescribeScript
--
-- -   UpdateScript
--
-- -   DeleteScript
module Network.AWS.GameLift.CreateScript
  ( -- * Creating a Request
    CreateScript (..),
    newCreateScript,

    -- * Request Lenses
    createScript_zipFile,
    createScript_version,
    createScript_name,
    createScript_storageLocation,
    createScript_tags,

    -- * Destructuring the Response
    CreateScriptResponse (..),
    newCreateScriptResponse,

    -- * Response Lenses
    createScriptResponse_script,
    createScriptResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | A data object containing your Realtime scripts and dependencies as a zip
    -- file. The zip file can have one or multiple files. Maximum size of a zip
    -- file is 5 MB.
    --
    -- When using the AWS CLI tool to create a script, this parameter is set to
    -- the zip file name. It must be prepended with the string \"fileb:\/\/\"
    -- to indicate that the file data is a binary object. For example:
    -- @--zip-file fileb:\/\/myRealtimeScript.zip@.
    zipFile :: Prelude.Maybe Core.Base64,
    -- | The version that is associated with a build or script. Version strings
    -- do not need to be unique. You can use UpdateScript to change this value
    -- later.
    version :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique. You can use UpdateScript to change this value
    -- later.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of your Realtime scripts. The storage location
    -- must specify the S3 bucket name, the zip file name (the \"key\"), and an
    -- IAM role ARN that allows Amazon GameLift to access the S3 storage
    -- location. The S3 bucket must be in the same Region where you are
    -- creating a new script. By default, Amazon GameLift uploads the latest
    -- version of the zip file; if you have S3 object versioning turned on, you
    -- can use the @ObjectVersion@ parameter to specify an earlier version. To
    -- call this operation with a storage location, you must have IAM PassRole
    -- permission. For more details on IAM roles and PassRole permissions, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
    storageLocation :: Prelude.Maybe S3Location,
    -- | A list of labels to assign to the new script resource. Tags are
    -- developer-defined key-value pairs. Tagging AWS resources are useful for
    -- resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zipFile', 'createScript_zipFile' - A data object containing your Realtime scripts and dependencies as a zip
-- file. The zip file can have one or multiple files. Maximum size of a zip
-- file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to
-- the zip file name. It must be prepended with the string \"fileb:\/\/\"
-- to indicate that the file data is a binary object. For example:
-- @--zip-file fileb:\/\/myRealtimeScript.zip@.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'version', 'createScript_version' - The version that is associated with a build or script. Version strings
-- do not need to be unique. You can use UpdateScript to change this value
-- later.
--
-- 'name', 'createScript_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique. You can use UpdateScript to change this value
-- later.
--
-- 'storageLocation', 'createScript_storageLocation' - The Amazon S3 location of your Realtime scripts. The storage location
-- must specify the S3 bucket name, the zip file name (the \"key\"), and an
-- IAM role ARN that allows Amazon GameLift to access the S3 storage
-- location. The S3 bucket must be in the same Region where you are
-- creating a new script. By default, Amazon GameLift uploads the latest
-- version of the zip file; if you have S3 object versioning turned on, you
-- can use the @ObjectVersion@ parameter to specify an earlier version. To
-- call this operation with a storage location, you must have IAM PassRole
-- permission. For more details on IAM roles and PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
--
-- 'tags', 'createScript_tags' - A list of labels to assign to the new script resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
newCreateScript ::
  CreateScript
newCreateScript =
  CreateScript'
    { zipFile = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A data object containing your Realtime scripts and dependencies as a zip
-- file. The zip file can have one or multiple files. Maximum size of a zip
-- file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to
-- the zip file name. It must be prepended with the string \"fileb:\/\/\"
-- to indicate that the file data is a binary object. For example:
-- @--zip-file fileb:\/\/myRealtimeScript.zip@.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createScript_zipFile :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.ByteString)
createScript_zipFile = Lens.lens (\CreateScript' {zipFile} -> zipFile) (\s@CreateScript' {} a -> s {zipFile = a} :: CreateScript) Prelude.. Lens.mapping Core._Base64

-- | The version that is associated with a build or script. Version strings
-- do not need to be unique. You can use UpdateScript to change this value
-- later.
createScript_version :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.Text)
createScript_version = Lens.lens (\CreateScript' {version} -> version) (\s@CreateScript' {} a -> s {version = a} :: CreateScript)

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique. You can use UpdateScript to change this value
-- later.
createScript_name :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.Text)
createScript_name = Lens.lens (\CreateScript' {name} -> name) (\s@CreateScript' {} a -> s {name = a} :: CreateScript)

-- | The Amazon S3 location of your Realtime scripts. The storage location
-- must specify the S3 bucket name, the zip file name (the \"key\"), and an
-- IAM role ARN that allows Amazon GameLift to access the S3 storage
-- location. The S3 bucket must be in the same Region where you are
-- creating a new script. By default, Amazon GameLift uploads the latest
-- version of the zip file; if you have S3 object versioning turned on, you
-- can use the @ObjectVersion@ parameter to specify an earlier version. To
-- call this operation with a storage location, you must have IAM PassRole
-- permission. For more details on IAM roles and PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
createScript_storageLocation :: Lens.Lens' CreateScript (Prelude.Maybe S3Location)
createScript_storageLocation = Lens.lens (\CreateScript' {storageLocation} -> storageLocation) (\s@CreateScript' {} a -> s {storageLocation = a} :: CreateScript)

-- | A list of labels to assign to the new script resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createScript_tags :: Lens.Lens' CreateScript (Prelude.Maybe [Tag])
createScript_tags = Lens.lens (\CreateScript' {tags} -> tags) (\s@CreateScript' {} a -> s {tags = a} :: CreateScript) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest CreateScript where
  type AWSResponse CreateScript = CreateScriptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Prelude.<$> (x Core..?> "Script")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScript

instance Prelude.NFData CreateScript

instance Core.ToHeaders CreateScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreateScript" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ZipFile" Core..=) Prelude.<$> zipFile,
            ("Version" Core..=) Prelude.<$> version,
            ("Name" Core..=) Prelude.<$> name,
            ("StorageLocation" Core..=)
              Prelude.<$> storageLocation,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateScript where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateScript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { -- | The newly created script record with a unique script ID and ARN. The new
    -- script\'s storage location reflects an Amazon S3 location: (1) If the
    -- script was uploaded from an S3 bucket under your account, the storage
    -- location reflects the information that was provided in the
    -- /CreateScript/ request; (2) If the script file was uploaded from a local
    -- zip file, the storage location reflects an S3 location controls by the
    -- Amazon GameLift service.
    script :: Prelude.Maybe Script,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'script', 'createScriptResponse_script' - The newly created script record with a unique script ID and ARN. The new
-- script\'s storage location reflects an Amazon S3 location: (1) If the
-- script was uploaded from an S3 bucket under your account, the storage
-- location reflects the information that was provided in the
-- /CreateScript/ request; (2) If the script file was uploaded from a local
-- zip file, the storage location reflects an S3 location controls by the
-- Amazon GameLift service.
--
-- 'httpStatus', 'createScriptResponse_httpStatus' - The response's http status code.
newCreateScriptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateScriptResponse
newCreateScriptResponse pHttpStatus_ =
  CreateScriptResponse'
    { script = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created script record with a unique script ID and ARN. The new
-- script\'s storage location reflects an Amazon S3 location: (1) If the
-- script was uploaded from an S3 bucket under your account, the storage
-- location reflects the information that was provided in the
-- /CreateScript/ request; (2) If the script file was uploaded from a local
-- zip file, the storage location reflects an S3 location controls by the
-- Amazon GameLift service.
createScriptResponse_script :: Lens.Lens' CreateScriptResponse (Prelude.Maybe Script)
createScriptResponse_script = Lens.lens (\CreateScriptResponse' {script} -> script) (\s@CreateScriptResponse' {} a -> s {script = a} :: CreateScriptResponse)

-- | The response's http status code.
createScriptResponse_httpStatus :: Lens.Lens' CreateScriptResponse Prelude.Int
createScriptResponse_httpStatus = Lens.lens (\CreateScriptResponse' {httpStatus} -> httpStatus) (\s@CreateScriptResponse' {} a -> s {httpStatus = a} :: CreateScriptResponse)

instance Prelude.NFData CreateScriptResponse
