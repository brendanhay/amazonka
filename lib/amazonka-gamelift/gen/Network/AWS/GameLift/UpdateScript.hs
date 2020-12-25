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
    usScriptId,
    usName,
    usStorageLocation,
    usVersion,
    usZipFile,

    -- * Destructuring the response
    UpdateScriptResponse (..),
    mkUpdateScriptResponse,

    -- ** Response lenses
    usrrsScript,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateScript' smart constructor.
data UpdateScript = UpdateScript'
  { -- | A unique identifier for a Realtime script to update. You can use either the script ID or ARN value.
    scriptId :: Types.ScriptIdOrArn,
    -- | A descriptive label that is associated with a script. Script names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region as the script you're updating. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
    storageLocation :: Core.Maybe Types.S3Location,
    -- | The version that is associated with a build or script. Version strings do not need to be unique.
    version :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
    --
    -- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
    zipFile :: Core.Maybe Core.Base64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateScript' value with any optional fields omitted.
mkUpdateScript ::
  -- | 'scriptId'
  Types.ScriptIdOrArn ->
  UpdateScript
mkUpdateScript scriptId =
  UpdateScript'
    { scriptId,
      name = Core.Nothing,
      storageLocation = Core.Nothing,
      version = Core.Nothing,
      zipFile = Core.Nothing
    }

-- | A unique identifier for a Realtime script to update. You can use either the script ID or ARN value.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usScriptId :: Lens.Lens' UpdateScript Types.ScriptIdOrArn
usScriptId = Lens.field @"scriptId"
{-# DEPRECATED usScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | A descriptive label that is associated with a script. Script names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateScript (Core.Maybe Types.NonZeroAndMaxString)
usName = Lens.field @"name"
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region as the script you're updating. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStorageLocation :: Lens.Lens' UpdateScript (Core.Maybe Types.S3Location)
usStorageLocation = Lens.field @"storageLocation"
{-# DEPRECATED usStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usVersion :: Lens.Lens' UpdateScript (Core.Maybe Types.NonZeroAndMaxString)
usVersion = Lens.field @"version"
{-# DEPRECATED usVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usZipFile :: Lens.Lens' UpdateScript (Core.Maybe Core.Base64)
usZipFile = Lens.field @"zipFile"
{-# DEPRECATED usZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

instance Core.FromJSON UpdateScript where
  toJSON UpdateScript {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ScriptId" Core..= scriptId),
            ("Name" Core..=) Core.<$> name,
            ("StorageLocation" Core..=) Core.<$> storageLocation,
            ("Version" Core..=) Core.<$> version,
            ("ZipFile" Core..=) Core.<$> zipFile
          ]
      )

instance Core.AWSRequest UpdateScript where
  type Rs UpdateScript = UpdateScriptResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateScript")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScriptResponse'
            Core.<$> (x Core..:? "Script") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateScriptResponse' smart constructor.
data UpdateScriptResponse = UpdateScriptResponse'
  { -- | The newly created script record with a unique script ID. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
    script :: Core.Maybe Types.Script,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateScriptResponse' value with any optional fields omitted.
mkUpdateScriptResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateScriptResponse
mkUpdateScriptResponse responseStatus =
  UpdateScriptResponse' {script = Core.Nothing, responseStatus}

-- | The newly created script record with a unique script ID. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsScript :: Lens.Lens' UpdateScriptResponse (Core.Maybe Types.Script)
usrrsScript = Lens.field @"script"
{-# DEPRECATED usrrsScript "Use generic-lens or generic-optics with 'script' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateScriptResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
