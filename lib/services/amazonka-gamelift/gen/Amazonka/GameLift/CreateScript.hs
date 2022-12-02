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
-- Module      : Amazonka.GameLift.CreateScript
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- -   An Amazon Simple Storage Service (Amazon S3) bucket under your
--     Amazon Web Services account. Use the /StorageLocation/ parameter for
--     this option. You\'ll need to have an Identity Access Management
--     (IAM) role that allows the Amazon GameLift service to access your S3
--     bucket.
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
-- __Related actions__
--
-- CreateScript | ListScripts | DescribeScript | UpdateScript |
-- DeleteScript |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateScript
  ( -- * Creating a Request
    CreateScript (..),
    newCreateScript,

    -- * Request Lenses
    createScript_tags,
    createScript_name,
    createScript_zipFile,
    createScript_storageLocation,
    createScript_version,

    -- * Destructuring the Response
    CreateScriptResponse (..),
    newCreateScriptResponse,

    -- * Response Lenses
    createScriptResponse_script,
    createScriptResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | A list of labels to assign to the new script resource. Tags are
    -- developer-defined key-value pairs. Tagging Amazon Web Services resources
    -- are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/. Once the resource is
    -- created, you can use TagResource, UntagResource, and ListTagsForResource
    -- to add, remove, and view tags. The maximum tag limit may be lower than
    -- stated. See the Amazon Web Services General Reference for actual tagging
    -- limits.
    tags :: Prelude.Maybe [Tag],
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique. You can use UpdateScript to change this value
    -- later.
    name :: Prelude.Maybe Prelude.Text,
    -- | A data object containing your Realtime scripts and dependencies as a zip
    -- file. The zip file can have one or multiple files. Maximum size of a zip
    -- file is 5 MB.
    --
    -- When using the Amazon Web Services CLI tool to create a script, this
    -- parameter is set to the zip file name. It must be prepended with the
    -- string \"fileb:\/\/\" to indicate that the file data is a binary object.
    -- For example: @--zip-file fileb:\/\/myRealtimeScript.zip@.
    zipFile :: Prelude.Maybe Data.Base64,
    -- | The location of the Amazon S3 bucket where a zipped file containing your
    -- Realtime scripts is stored. The storage location must specify the Amazon
    -- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
    -- allows Amazon GameLift to access the Amazon S3 storage location. The S3
    -- bucket must be in the same Region where you want to create a new script.
    -- By default, Amazon GameLift uploads the latest version of the zip file;
    -- if you have S3 object versioning turned on, you can use the
    -- @ObjectVersion@ parameter to specify an earlier version.
    storageLocation :: Prelude.Maybe S3Location,
    -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique. You can use UpdateScript to change
    -- this value later.
    version :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'createScript_tags' - A list of labels to assign to the new script resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the resource is
-- created, you can use TagResource, UntagResource, and ListTagsForResource
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the Amazon Web Services General Reference for actual tagging
-- limits.
--
-- 'name', 'createScript_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique. You can use UpdateScript to change this value
-- later.
--
-- 'zipFile', 'createScript_zipFile' - A data object containing your Realtime scripts and dependencies as a zip
-- file. The zip file can have one or multiple files. Maximum size of a zip
-- file is 5 MB.
--
-- When using the Amazon Web Services CLI tool to create a script, this
-- parameter is set to the zip file name. It must be prepended with the
-- string \"fileb:\/\/\" to indicate that the file data is a binary object.
-- For example: @--zip-file fileb:\/\/myRealtimeScript.zip@.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'storageLocation', 'createScript_storageLocation' - The location of the Amazon S3 bucket where a zipped file containing your
-- Realtime scripts is stored. The storage location must specify the Amazon
-- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
-- allows Amazon GameLift to access the Amazon S3 storage location. The S3
-- bucket must be in the same Region where you want to create a new script.
-- By default, Amazon GameLift uploads the latest version of the zip file;
-- if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version.
--
-- 'version', 'createScript_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique. You can use UpdateScript to change
-- this value later.
newCreateScript ::
  CreateScript
newCreateScript =
  CreateScript'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A list of labels to assign to the new script resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the resource is
-- created, you can use TagResource, UntagResource, and ListTagsForResource
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the Amazon Web Services General Reference for actual tagging
-- limits.
createScript_tags :: Lens.Lens' CreateScript (Prelude.Maybe [Tag])
createScript_tags = Lens.lens (\CreateScript' {tags} -> tags) (\s@CreateScript' {} a -> s {tags = a} :: CreateScript) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique. You can use UpdateScript to change this value
-- later.
createScript_name :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.Text)
createScript_name = Lens.lens (\CreateScript' {name} -> name) (\s@CreateScript' {} a -> s {name = a} :: CreateScript)

-- | A data object containing your Realtime scripts and dependencies as a zip
-- file. The zip file can have one or multiple files. Maximum size of a zip
-- file is 5 MB.
--
-- When using the Amazon Web Services CLI tool to create a script, this
-- parameter is set to the zip file name. It must be prepended with the
-- string \"fileb:\/\/\" to indicate that the file data is a binary object.
-- For example: @--zip-file fileb:\/\/myRealtimeScript.zip@.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createScript_zipFile :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.ByteString)
createScript_zipFile = Lens.lens (\CreateScript' {zipFile} -> zipFile) (\s@CreateScript' {} a -> s {zipFile = a} :: CreateScript) Prelude.. Lens.mapping Data._Base64

-- | The location of the Amazon S3 bucket where a zipped file containing your
-- Realtime scripts is stored. The storage location must specify the Amazon
-- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
-- allows Amazon GameLift to access the Amazon S3 storage location. The S3
-- bucket must be in the same Region where you want to create a new script.
-- By default, Amazon GameLift uploads the latest version of the zip file;
-- if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version.
createScript_storageLocation :: Lens.Lens' CreateScript (Prelude.Maybe S3Location)
createScript_storageLocation = Lens.lens (\CreateScript' {storageLocation} -> storageLocation) (\s@CreateScript' {} a -> s {storageLocation = a} :: CreateScript)

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique. You can use UpdateScript to change
-- this value later.
createScript_version :: Lens.Lens' CreateScript (Prelude.Maybe Prelude.Text)
createScript_version = Lens.lens (\CreateScript' {version} -> version) (\s@CreateScript' {} a -> s {version = a} :: CreateScript)

instance Core.AWSRequest CreateScript where
  type AWSResponse CreateScript = CreateScriptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Prelude.<$> (x Data..?> "Script")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScript where
  hashWithSalt _salt CreateScript' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` zipFile
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` version

instance Prelude.NFData CreateScript where
  rnf CreateScript' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf zipFile
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders CreateScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateScript" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Name" Data..=) Prelude.<$> name,
            ("ZipFile" Data..=) Prelude.<$> zipFile,
            ("StorageLocation" Data..=)
              Prelude.<$> storageLocation,
            ("Version" Data..=) Prelude.<$> version
          ]
      )

instance Data.ToPath CreateScript where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateScript where
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

instance Prelude.NFData CreateScriptResponse where
  rnf CreateScriptResponse' {..} =
    Prelude.rnf script
      `Prelude.seq` Prelude.rnf httpStatus
