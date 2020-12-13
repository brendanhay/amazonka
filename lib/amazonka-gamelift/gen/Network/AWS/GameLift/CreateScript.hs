{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new script record for your Realtime Servers script. Realtime scripts are JavaScript that provide configuration settings and optional custom game logic for your game. The script is deployed when you create a Realtime Servers fleet to host your game sessions. Script logic is executed during an active game session.
--
-- To create a new script record, specify a script name and provide the script file(s). The script files and all dependencies must be zipped into a single file. You can pull the zip file from either of these locations:
--
--     * A locally available directory. Use the /ZipFile/ parameter for this option.
--
--
--     * An Amazon Simple Storage Service (Amazon S3) bucket under your AWS account. Use the /StorageLocation/ parameter for this option. You'll need to have an Identity Access Management (IAM) role that allows the Amazon GameLift service to access your S3 bucket.
--
--
-- If the call is successful, a new script record is created with a unique script ID. If the script file is provided as a local file, the file is uploaded to an Amazon GameLift-owned S3 bucket and the script record's storage location reflects this location. If the script file is provided as an S3 bucket, Amazon GameLift accesses the file at this storage location as needed for deployment.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set Up a Role for Amazon GameLift Access>
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
module Network.AWS.GameLift.CreateScript
  ( -- * Creating a request
    CreateScript (..),
    mkCreateScript,

    -- ** Request lenses
    csStorageLocation,
    csZipFile,
    csName,
    csVersion,
    csTags,

    -- * Destructuring the response
    CreateScriptResponse (..),
    mkCreateScriptResponse,

    -- ** Response lenses
    csrsScript,
    csrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
    storageLocation :: Lude.Maybe S3Location,
    -- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
    --
    -- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
    zipFile :: Lude.Maybe Lude.Base64,
    -- | A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later.
    name :: Lude.Maybe Lude.Text,
    -- | The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later.
    version :: Lude.Maybe Lude.Text,
    -- | A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScript' with the minimum fields required to make a request.
--
-- * 'storageLocation' - The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
-- * 'zipFile' - A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
-- * 'name' - A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later.
-- * 'version' - The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later.
-- * 'tags' - A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
mkCreateScript ::
  CreateScript
mkCreateScript =
  CreateScript'
    { storageLocation = Lude.Nothing,
      zipFile = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStorageLocation :: Lens.Lens' CreateScript (Lude.Maybe S3Location)
csStorageLocation = Lens.lens (storageLocation :: CreateScript -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: CreateScript)
{-# DEPRECATED csStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csZipFile :: Lens.Lens' CreateScript (Lude.Maybe Lude.Base64)
csZipFile = Lens.lens (zipFile :: CreateScript -> Lude.Maybe Lude.Base64) (\s a -> s {zipFile = a} :: CreateScript)
{-# DEPRECATED csZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

-- | A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateScript (Lude.Maybe Lude.Text)
csName = Lens.lens (name :: CreateScript -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateScript)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVersion :: Lens.Lens' CreateScript (Lude.Maybe Lude.Text)
csVersion = Lens.lens (version :: CreateScript -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateScript)
{-# DEPRECATED csVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateScript (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateScript -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateScript)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateScript where
  type Rs CreateScript = CreateScriptResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Lude.<$> (x Lude..?> "Script") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateScript where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateScript" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StorageLocation" Lude..=) Lude.<$> storageLocation,
            ("ZipFile" Lude..=) Lude.<$> zipFile,
            ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateScript where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateScript where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { -- | The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
    script :: Lude.Maybe Script,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScriptResponse' with the minimum fields required to make a request.
--
-- * 'script' - The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
-- * 'responseStatus' - The response status code.
mkCreateScriptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateScriptResponse
mkCreateScriptResponse pResponseStatus_ =
  CreateScriptResponse'
    { script = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsScript :: Lens.Lens' CreateScriptResponse (Lude.Maybe Script)
csrsScript = Lens.lens (script :: CreateScriptResponse -> Lude.Maybe Script) (\s a -> s {script = a} :: CreateScriptResponse)
{-# DEPRECATED csrsScript "Use generic-lens or generic-optics with 'script' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateScriptResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateScriptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateScriptResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
