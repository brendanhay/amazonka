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
-- Module      : Network.AWS.GameLift.UpdateScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates Realtime script metadata and content.
--
-- To update script metadata, specify the script ID and provide updated
-- name and\/or version values.
--
-- To update script content, provide an updated zip file by pointing to
-- either a local file or an Amazon S3 bucket location. You can use either
-- method regardless of how the original script was uploaded. Use the
-- /Version/ parameter to track updates to the script.
--
-- If the call is successful, the updated metadata is stored in the script
-- record and a revised script is uploaded to the Amazon GameLift service.
-- Once the script is updated and acquired by a fleet instance, the new
-- version is used for all new game sessions.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
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
module Network.AWS.GameLift.UpdateScript
  ( -- * Creating a Request
    UpdateScript (..),
    newUpdateScript,

    -- * Request Lenses
    updateScript_zipFile,
    updateScript_version,
    updateScript_name,
    updateScript_storageLocation,
    updateScript_scriptId,

    -- * Destructuring the Response
    UpdateScriptResponse (..),
    newUpdateScriptResponse,

    -- * Response Lenses
    updateScriptResponse_script,
    updateScriptResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateScript' smart constructor.
data UpdateScript = UpdateScript'
  { -- | A data object containing your Realtime scripts and dependencies as a zip
    -- file. The zip file can have one or multiple files. Maximum size of a zip
    -- file is 5 MB.
    --
    -- When using the AWS CLI tool to create a script, this parameter is set to
    -- the zip file name. It must be prepended with the string \"fileb:\/\/\"
    -- to indicate that the file data is a binary object. For example:
    -- @--zip-file fileb:\/\/myRealtimeScript.zip@.
    zipFile :: Core.Maybe Core.Base64,
    -- | The version that is associated with a build or script. Version strings
    -- do not need to be unique.
    version :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique.
    name :: Core.Maybe Core.Text,
    -- | The Amazon S3 location of your Realtime scripts. The storage location
    -- must specify the S3 bucket name, the zip file name (the \"key\"), and an
    -- IAM role ARN that allows Amazon GameLift to access the S3 storage
    -- location. The S3 bucket must be in the same Region as the script you\'re
    -- updating. By default, Amazon GameLift uploads the latest version of the
    -- zip file; if you have S3 object versioning turned on, you can use the
    -- @ObjectVersion@ parameter to specify an earlier version. To call this
    -- operation with a storage location, you must have IAM PassRole
    -- permission. For more details on IAM roles and PassRole permissions, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
    storageLocation :: Core.Maybe S3Location,
    -- | A unique identifier for a Realtime script to update. You can use either
    -- the script ID or ARN value.
    scriptId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zipFile', 'updateScript_zipFile' - A data object containing your Realtime scripts and dependencies as a zip
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
-- 'version', 'updateScript_version' - The version that is associated with a build or script. Version strings
-- do not need to be unique.
--
-- 'name', 'updateScript_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique.
--
-- 'storageLocation', 'updateScript_storageLocation' - The Amazon S3 location of your Realtime scripts. The storage location
-- must specify the S3 bucket name, the zip file name (the \"key\"), and an
-- IAM role ARN that allows Amazon GameLift to access the S3 storage
-- location. The S3 bucket must be in the same Region as the script you\'re
-- updating. By default, Amazon GameLift uploads the latest version of the
-- zip file; if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version. To call this
-- operation with a storage location, you must have IAM PassRole
-- permission. For more details on IAM roles and PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
--
-- 'scriptId', 'updateScript_scriptId' - A unique identifier for a Realtime script to update. You can use either
-- the script ID or ARN value.
newUpdateScript ::
  -- | 'scriptId'
  Core.Text ->
  UpdateScript
newUpdateScript pScriptId_ =
  UpdateScript'
    { zipFile = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      storageLocation = Core.Nothing,
      scriptId = pScriptId_
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
updateScript_zipFile :: Lens.Lens' UpdateScript (Core.Maybe Core.ByteString)
updateScript_zipFile = Lens.lens (\UpdateScript' {zipFile} -> zipFile) (\s@UpdateScript' {} a -> s {zipFile = a} :: UpdateScript) Core.. Lens.mapping Core._Base64

-- | The version that is associated with a build or script. Version strings
-- do not need to be unique.
updateScript_version :: Lens.Lens' UpdateScript (Core.Maybe Core.Text)
updateScript_version = Lens.lens (\UpdateScript' {version} -> version) (\s@UpdateScript' {} a -> s {version = a} :: UpdateScript)

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique.
updateScript_name :: Lens.Lens' UpdateScript (Core.Maybe Core.Text)
updateScript_name = Lens.lens (\UpdateScript' {name} -> name) (\s@UpdateScript' {} a -> s {name = a} :: UpdateScript)

-- | The Amazon S3 location of your Realtime scripts. The storage location
-- must specify the S3 bucket name, the zip file name (the \"key\"), and an
-- IAM role ARN that allows Amazon GameLift to access the S3 storage
-- location. The S3 bucket must be in the same Region as the script you\'re
-- updating. By default, Amazon GameLift uploads the latest version of the
-- zip file; if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version. To call this
-- operation with a storage location, you must have IAM PassRole
-- permission. For more details on IAM roles and PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
updateScript_storageLocation :: Lens.Lens' UpdateScript (Core.Maybe S3Location)
updateScript_storageLocation = Lens.lens (\UpdateScript' {storageLocation} -> storageLocation) (\s@UpdateScript' {} a -> s {storageLocation = a} :: UpdateScript)

-- | A unique identifier for a Realtime script to update. You can use either
-- the script ID or ARN value.
updateScript_scriptId :: Lens.Lens' UpdateScript Core.Text
updateScript_scriptId = Lens.lens (\UpdateScript' {scriptId} -> scriptId) (\s@UpdateScript' {} a -> s {scriptId = a} :: UpdateScript)

instance Core.AWSRequest UpdateScript where
  type AWSResponse UpdateScript = UpdateScriptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScriptResponse'
            Core.<$> (x Core..?> "Script")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateScript

instance Core.NFData UpdateScript

instance Core.ToHeaders UpdateScript where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.UpdateScript" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateScript where
  toJSON UpdateScript' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ZipFile" Core..=) Core.<$> zipFile,
            ("Version" Core..=) Core.<$> version,
            ("Name" Core..=) Core.<$> name,
            ("StorageLocation" Core..=) Core.<$> storageLocation,
            Core.Just ("ScriptId" Core..= scriptId)
          ]
      )

instance Core.ToPath UpdateScript where
  toPath = Core.const "/"

instance Core.ToQuery UpdateScript where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateScriptResponse' smart constructor.
data UpdateScriptResponse = UpdateScriptResponse'
  { -- | The newly created script record with a unique script ID. The new
    -- script\'s storage location reflects an Amazon S3 location: (1) If the
    -- script was uploaded from an S3 bucket under your account, the storage
    -- location reflects the information that was provided in the
    -- /CreateScript/ request; (2) If the script file was uploaded from a local
    -- zip file, the storage location reflects an S3 location controls by the
    -- Amazon GameLift service.
    script :: Core.Maybe Script,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'script', 'updateScriptResponse_script' - The newly created script record with a unique script ID. The new
-- script\'s storage location reflects an Amazon S3 location: (1) If the
-- script was uploaded from an S3 bucket under your account, the storage
-- location reflects the information that was provided in the
-- /CreateScript/ request; (2) If the script file was uploaded from a local
-- zip file, the storage location reflects an S3 location controls by the
-- Amazon GameLift service.
--
-- 'httpStatus', 'updateScriptResponse_httpStatus' - The response's http status code.
newUpdateScriptResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateScriptResponse
newUpdateScriptResponse pHttpStatus_ =
  UpdateScriptResponse'
    { script = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created script record with a unique script ID. The new
-- script\'s storage location reflects an Amazon S3 location: (1) If the
-- script was uploaded from an S3 bucket under your account, the storage
-- location reflects the information that was provided in the
-- /CreateScript/ request; (2) If the script file was uploaded from a local
-- zip file, the storage location reflects an S3 location controls by the
-- Amazon GameLift service.
updateScriptResponse_script :: Lens.Lens' UpdateScriptResponse (Core.Maybe Script)
updateScriptResponse_script = Lens.lens (\UpdateScriptResponse' {script} -> script) (\s@UpdateScriptResponse' {} a -> s {script = a} :: UpdateScriptResponse)

-- | The response's http status code.
updateScriptResponse_httpStatus :: Lens.Lens' UpdateScriptResponse Core.Int
updateScriptResponse_httpStatus = Lens.lens (\UpdateScriptResponse' {httpStatus} -> httpStatus) (\s@UpdateScriptResponse' {} a -> s {httpStatus = a} :: UpdateScriptResponse)

instance Core.NFData UpdateScriptResponse
