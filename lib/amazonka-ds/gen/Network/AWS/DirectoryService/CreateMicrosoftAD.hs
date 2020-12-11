{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateMicrosoftAD
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Microsoft AD directory in the AWS Cloud. For more information, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Managed Microsoft AD> in the /AWS Directory Service Admin Guide/ .
--
-- Before you call /CreateMicrosoftAD/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /CreateMicrosoftAD/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.CreateMicrosoftAD
  ( -- * Creating a request
    CreateMicrosoftAD (..),
    mkCreateMicrosoftAD,

    -- ** Request lenses
    cmadEdition,
    cmadShortName,
    cmadDescription,
    cmadTags,
    cmadName,
    cmadPassword,
    cmadVPCSettings,

    -- * Destructuring the response
    CreateMicrosoftADResponse (..),
    mkCreateMicrosoftADResponse,

    -- ** Response lenses
    cmadrsDirectoryId,
    cmadrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates an AWS Managed Microsoft AD directory.
--
-- /See:/ 'mkCreateMicrosoftAD' smart constructor.
data CreateMicrosoftAD = CreateMicrosoftAD'
  { edition ::
      Lude.Maybe DirectoryEdition,
    shortName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    password :: Lude.Sensitive Lude.Text,
    vpcSettings :: DirectoryVPCSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMicrosoftAD' with the minimum fields required to make a request.
--
-- * 'description' - A description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
-- * 'edition' - AWS Managed Microsoft AD is available in two editions: @Standard@ and @Enterprise@ . @Enterprise@ is the default.
-- * 'name' - The fully qualified domain name for the AWS Managed Microsoft AD directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
-- * 'password' - The password for the default administrative user named @Admin@ .
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
-- * 'shortName' - The NetBIOS name for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ .
-- * 'tags' - The tags to be assigned to the AWS Managed Microsoft AD directory.
-- * 'vpcSettings' - Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
mkCreateMicrosoftAD ::
  -- | 'name'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  -- | 'vpcSettings'
  DirectoryVPCSettings ->
  CreateMicrosoftAD
mkCreateMicrosoftAD pName_ pPassword_ pVPCSettings_ =
  CreateMicrosoftAD'
    { edition = Lude.Nothing,
      shortName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      password = pPassword_,
      vpcSettings = pVPCSettings_
    }

-- | AWS Managed Microsoft AD is available in two editions: @Standard@ and @Enterprise@ . @Enterprise@ is the default.
--
-- /Note:/ Consider using 'edition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadEdition :: Lens.Lens' CreateMicrosoftAD (Lude.Maybe DirectoryEdition)
cmadEdition = Lens.lens (edition :: CreateMicrosoftAD -> Lude.Maybe DirectoryEdition) (\s a -> s {edition = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadEdition "Use generic-lens or generic-optics with 'edition' instead." #-}

-- | The NetBIOS name for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ .
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadShortName :: Lens.Lens' CreateMicrosoftAD (Lude.Maybe Lude.Text)
cmadShortName = Lens.lens (shortName :: CreateMicrosoftAD -> Lude.Maybe Lude.Text) (\s a -> s {shortName = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | A description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadDescription :: Lens.Lens' CreateMicrosoftAD (Lude.Maybe Lude.Text)
cmadDescription = Lens.lens (description :: CreateMicrosoftAD -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to be assigned to the AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadTags :: Lens.Lens' CreateMicrosoftAD (Lude.Maybe [Tag])
cmadTags = Lens.lens (tags :: CreateMicrosoftAD -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The fully qualified domain name for the AWS Managed Microsoft AD directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadName :: Lens.Lens' CreateMicrosoftAD Lude.Text
cmadName = Lens.lens (name :: CreateMicrosoftAD -> Lude.Text) (\s a -> s {name = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The password for the default administrative user named @Admin@ .
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadPassword :: Lens.Lens' CreateMicrosoftAD (Lude.Sensitive Lude.Text)
cmadPassword = Lens.lens (password :: CreateMicrosoftAD -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadVPCSettings :: Lens.Lens' CreateMicrosoftAD DirectoryVPCSettings
cmadVPCSettings = Lens.lens (vpcSettings :: CreateMicrosoftAD -> DirectoryVPCSettings) (\s a -> s {vpcSettings = a} :: CreateMicrosoftAD)
{-# DEPRECATED cmadVPCSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

instance Lude.AWSRequest CreateMicrosoftAD where
  type Rs CreateMicrosoftAD = CreateMicrosoftADResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMicrosoftADResponse'
            Lude.<$> (x Lude..?> "DirectoryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMicrosoftAD where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.CreateMicrosoftAD" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMicrosoftAD where
  toJSON CreateMicrosoftAD' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Edition" Lude..=) Lude.<$> edition,
            ("ShortName" Lude..=) Lude.<$> shortName,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Password" Lude..= password),
            Lude.Just ("VpcSettings" Lude..= vpcSettings)
          ]
      )

instance Lude.ToPath CreateMicrosoftAD where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMicrosoftAD where
  toQuery = Lude.const Lude.mempty

-- | Result of a CreateMicrosoftAD request.
--
-- /See:/ 'mkCreateMicrosoftADResponse' smart constructor.
data CreateMicrosoftADResponse = CreateMicrosoftADResponse'
  { directoryId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMicrosoftADResponse' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory that was created.
-- * 'responseStatus' - The response status code.
mkCreateMicrosoftADResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMicrosoftADResponse
mkCreateMicrosoftADResponse pResponseStatus_ =
  CreateMicrosoftADResponse'
    { directoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the directory that was created.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadrsDirectoryId :: Lens.Lens' CreateMicrosoftADResponse (Lude.Maybe Lude.Text)
cmadrsDirectoryId = Lens.lens (directoryId :: CreateMicrosoftADResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: CreateMicrosoftADResponse)
{-# DEPRECATED cmadrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadrsResponseStatus :: Lens.Lens' CreateMicrosoftADResponse Lude.Int
cmadrsResponseStatus = Lens.lens (responseStatus :: CreateMicrosoftADResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMicrosoftADResponse)
{-# DEPRECATED cmadrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
