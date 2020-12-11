{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information regarding a given organization based on its identifier.
module Network.AWS.WorkMail.DescribeOrganization
  ( -- * Creating a request
    DescribeOrganization (..),
    mkDescribeOrganization,

    -- ** Request lenses
    desOrganizationId,

    -- * Destructuring the response
    DescribeOrganizationResponse (..),
    mkDescribeOrganizationResponse,

    -- ** Response lenses
    dorsDirectoryId,
    dorsState,
    dorsARN,
    dorsAlias,
    dorsCompletedDate,
    dorsDirectoryType,
    dorsDefaultMailDomain,
    dorsErrorMessage,
    dorsOrganizationId,
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeOrganization' smart constructor.
newtype DescribeOrganization = DescribeOrganization'
  { organizationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganization' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier for the organization to be described.
mkDescribeOrganization ::
  -- | 'organizationId'
  Lude.Text ->
  DescribeOrganization
mkDescribeOrganization pOrganizationId_ =
  DescribeOrganization' {organizationId = pOrganizationId_}

-- | The identifier for the organization to be described.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desOrganizationId :: Lens.Lens' DescribeOrganization Lude.Text
desOrganizationId = Lens.lens (organizationId :: DescribeOrganization -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeOrganization)
{-# DEPRECATED desOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DescribeOrganization where
  type Rs DescribeOrganization = DescribeOrganizationResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Lude.<$> (x Lude..?> "DirectoryId")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Alias")
            Lude.<*> (x Lude..?> "CompletedDate")
            Lude.<*> (x Lude..?> "DirectoryType")
            Lude.<*> (x Lude..?> "DefaultMailDomain")
            Lude.<*> (x Lude..?> "ErrorMessage")
            Lude.<*> (x Lude..?> "OrganizationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DescribeOrganization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganization where
  toJSON DescribeOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("OrganizationId" Lude..= organizationId)]
      )

instance Lude.ToPath DescribeOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { directoryId ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    alias :: Lude.Maybe Lude.Text,
    completedDate ::
      Lude.Maybe Lude.Timestamp,
    directoryType ::
      Lude.Maybe Lude.Text,
    defaultMailDomain ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
    organizationId ::
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

-- | Creates a value of 'DescribeOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'alias' - The alias for an organization.
-- * 'arn' - The Amazon Resource Name (ARN) of the organization.
-- * 'completedDate' - The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
-- * 'defaultMailDomain' - The default mail domain associated with the organization.
-- * 'directoryId' - The identifier for the directory associated with an Amazon WorkMail organization.
-- * 'directoryType' - The type of directory associated with the WorkMail organization.
-- * 'errorMessage' - (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
-- * 'organizationId' - The identifier of an organization.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of an organization.
mkDescribeOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationResponse
mkDescribeOrganizationResponse pResponseStatus_ =
  DescribeOrganizationResponse'
    { directoryId = Lude.Nothing,
      state = Lude.Nothing,
      arn = Lude.Nothing,
      alias = Lude.Nothing,
      completedDate = Lude.Nothing,
      directoryType = Lude.Nothing,
      defaultMailDomain = Lude.Nothing,
      errorMessage = Lude.Nothing,
      organizationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the directory associated with an Amazon WorkMail organization.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDirectoryId :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsDirectoryId = Lens.lens (directoryId :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The state of an organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsState :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsState = Lens.lens (state :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the organization.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsARN :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsARN = Lens.lens (arn :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The alias for an organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsAlias :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsAlias = Lens.lens (alias :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {alias = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
--
-- /Note:/ Consider using 'completedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsCompletedDate :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Timestamp)
dorsCompletedDate = Lens.lens (completedDate :: DescribeOrganizationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedDate = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsCompletedDate "Use generic-lens or generic-optics with 'completedDate' instead." #-}

-- | The type of directory associated with the WorkMail organization.
--
-- /Note:/ Consider using 'directoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDirectoryType :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsDirectoryType = Lens.lens (directoryType :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryType = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsDirectoryType "Use generic-lens or generic-optics with 'directoryType' instead." #-}

-- | The default mail domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDefaultMailDomain :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsDefaultMailDomain = Lens.lens (defaultMailDomain :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultMailDomain = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsDefaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead." #-}

-- | (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsErrorMessage :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsErrorMessage = Lens.lens (errorMessage :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The identifier of an organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsOrganizationId :: Lens.Lens' DescribeOrganizationResponse (Lude.Maybe Lude.Text)
dorsOrganizationId = Lens.lens (organizationId :: DescribeOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DescribeOrganizationResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
