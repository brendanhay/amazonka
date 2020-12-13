{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates Realtime script metadata and content.
--
-- To update script metadata, specify the script ID and provide updated name and/or version values.
-- To update script content, provide an updated zip file by pointing to either a local file or an Amazon S3 bucket location. You can use either method regardless of how the original script was uploaded. Use the /Version/ parameter to track updates to the script.
-- If the call is successful, the updated metadata is stored in the script record and a revised script is uploaded to the Amazon GameLift service. Once the script is updated and acquired by a fleet instance, the new version is used for all new game sessions.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
-- __Related operations__
--
--     * 'CreateScript'
--
--
--     * 'ListScripts'
--
--
--     * 'DescribeScript'
--
--
--     * 'UpdateScript'
--
--
--     * 'DeleteScript'
module Network.AWS.GameLift.UpdateScript
  ( -- * Creating a request
    UpdateScript (..),
    mkUpdateScript,

    -- ** Request lenses
    usStorageLocation,
    usZipFile,
    usName,
    usScriptId,
    usVersion,

    -- * Destructuring the response
    UpdateScriptResponse (..),
    mkUpdateScriptResponse,

    -- ** Response lenses
    usrsScript,
    usrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateScript' smart constructor.
data UpdateScript = UpdateScript'
  { -- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region as the script you're updating. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
    storageLocation :: Lude.Maybe S3Location,
    -- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
    --
    -- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
    zipFile :: Lude.Maybe Lude.Base64,
    -- | A descriptive label that is associated with a script. Script names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a Realtime script to update. You can use either the script ID or ARN value.
    scriptId :: Lude.Text,
    -- | The version that is associated with a build or script. Version strings do not need to be unique.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScript' with the minimum fields required to make a request.
--
-- * 'storageLocation' - The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region as the script you're updating. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
-- * 'zipFile' - A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
-- * 'name' - A descriptive label that is associated with a script. Script names do not need to be unique.
-- * 'scriptId' - A unique identifier for a Realtime script to update. You can use either the script ID or ARN value.
-- * 'version' - The version that is associated with a build or script. Version strings do not need to be unique.
mkUpdateScript ::
  -- | 'scriptId'
  Lude.Text ->
  UpdateScript
mkUpdateScript pScriptId_ =
  UpdateScript'
    { storageLocation = Lude.Nothing,
      zipFile = Lude.Nothing,
      name = Lude.Nothing,
      scriptId = pScriptId_,
      version = Lude.Nothing
    }

-- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region as the script you're updating. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStorageLocation :: Lens.Lens' UpdateScript (Lude.Maybe S3Location)
usStorageLocation = Lens.lens (storageLocation :: UpdateScript -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: UpdateScript)
{-# DEPRECATED usStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usZipFile :: Lens.Lens' UpdateScript (Lude.Maybe Lude.Base64)
usZipFile = Lens.lens (zipFile :: UpdateScript -> Lude.Maybe Lude.Base64) (\s a -> s {zipFile = a} :: UpdateScript)
{-# DEPRECATED usZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

-- | A descriptive label that is associated with a script. Script names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateScript (Lude.Maybe Lude.Text)
usName = Lens.lens (name :: UpdateScript -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateScript)
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a Realtime script to update. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usScriptId :: Lens.Lens' UpdateScript Lude.Text
usScriptId = Lens.lens (scriptId :: UpdateScript -> Lude.Text) (\s a -> s {scriptId = a} :: UpdateScript)
{-# DEPRECATED usScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usVersion :: Lens.Lens' UpdateScript (Lude.Maybe Lude.Text)
usVersion = Lens.lens (version :: UpdateScript -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateScript)
{-# DEPRECATED usVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest UpdateScript where
  type Rs UpdateScript = UpdateScriptResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateScriptResponse'
            Lude.<$> (x Lude..?> "Script") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateScript where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateScript" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateScript where
  toJSON UpdateScript' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StorageLocation" Lude..=) Lude.<$> storageLocation,
            ("ZipFile" Lude..=) Lude.<$> zipFile,
            ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("ScriptId" Lude..= scriptId),
            ("Version" Lude..=) Lude.<$> version
          ]
      )

instance Lude.ToPath UpdateScript where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateScript where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateScriptResponse' smart constructor.
data UpdateScriptResponse = UpdateScriptResponse'
  { -- | The newly created script record with a unique script ID. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
    script :: Lude.Maybe Script,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScriptResponse' with the minimum fields required to make a request.
--
-- * 'script' - The newly created script record with a unique script ID. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
-- * 'responseStatus' - The response status code.
mkUpdateScriptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateScriptResponse
mkUpdateScriptResponse pResponseStatus_ =
  UpdateScriptResponse'
    { script = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created script record with a unique script ID. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsScript :: Lens.Lens' UpdateScriptResponse (Lude.Maybe Script)
usrsScript = Lens.lens (script :: UpdateScriptResponse -> Lude.Maybe Script) (\s a -> s {script = a} :: UpdateScriptResponse)
{-# DEPRECATED usrsScript "Use generic-lens or generic-optics with 'script' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateScriptResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateScriptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateScriptResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
