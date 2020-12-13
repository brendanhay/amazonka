{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IPSet, which is called a trusted IP list in the console user interface. An IPSet is a list of IP addresses that are trusted for secure communication with AWS infrastructure and applications. GuardDuty doesn't generate findings for IP addresses that are included in IPSets. Only users from the master account can use this operation.
module Network.AWS.GuardDuty.CreateIPSet
  ( -- * Creating a request
    CreateIPSet (..),
    mkCreateIPSet,

    -- ** Request lenses
    cisClientToken,
    cisLocation,
    cisFormat,
    cisActivate,
    cisDetectorId,
    cisName,
    cisTags,

    -- * Destructuring the response
    CreateIPSetResponse (..),
    mkCreateIPSetResponse,

    -- ** Response lenses
    cisrsIPSetId,
    cisrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | The idempotency token for the create request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
    location :: Lude.Text,
    -- | The format of the file that contains the IPSet.
    format :: IPSetFormat,
    -- | A Boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
    activate :: Lude.Bool,
    -- | The unique ID of the detector of the GuardDuty account that you want to create an IPSet for.
    detectorId :: Lude.Text,
    -- | The user-friendly name to identify the IPSet.
    --
    -- Allowed characters are alphanumerics, spaces, hyphens (-), and underscores (_).
    name :: Lude.Text,
    -- | The tags to be added to a new IP set resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIPSet' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the create request.
-- * 'location' - The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
-- * 'format' - The format of the file that contains the IPSet.
-- * 'activate' - A Boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account that you want to create an IPSet for.
-- * 'name' - The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and underscores (_).
-- * 'tags' - The tags to be added to a new IP set resource.
mkCreateIPSet ::
  -- | 'location'
  Lude.Text ->
  -- | 'format'
  IPSetFormat ->
  -- | 'activate'
  Lude.Bool ->
  -- | 'detectorId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateIPSet
mkCreateIPSet pLocation_ pFormat_ pActivate_ pDetectorId_ pName_ =
  CreateIPSet'
    { clientToken = Lude.Nothing,
      location = pLocation_,
      format = pFormat_,
      activate = pActivate_,
      detectorId = pDetectorId_,
      name = pName_,
      tags = Lude.Nothing
    }

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisClientToken :: Lens.Lens' CreateIPSet (Lude.Maybe Lude.Text)
cisClientToken = Lens.lens (clientToken :: CreateIPSet -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateIPSet)
{-# DEPRECATED cisClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisLocation :: Lens.Lens' CreateIPSet Lude.Text
cisLocation = Lens.lens (location :: CreateIPSet -> Lude.Text) (\s a -> s {location = a} :: CreateIPSet)
{-# DEPRECATED cisLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The format of the file that contains the IPSet.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisFormat :: Lens.Lens' CreateIPSet IPSetFormat
cisFormat = Lens.lens (format :: CreateIPSet -> IPSetFormat) (\s a -> s {format = a} :: CreateIPSet)
{-# DEPRECATED cisFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | A Boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisActivate :: Lens.Lens' CreateIPSet Lude.Bool
cisActivate = Lens.lens (activate :: CreateIPSet -> Lude.Bool) (\s a -> s {activate = a} :: CreateIPSet)
{-# DEPRECATED cisActivate "Use generic-lens or generic-optics with 'activate' instead." #-}

-- | The unique ID of the detector of the GuardDuty account that you want to create an IPSet for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisDetectorId :: Lens.Lens' CreateIPSet Lude.Text
cisDetectorId = Lens.lens (detectorId :: CreateIPSet -> Lude.Text) (\s a -> s {detectorId = a} :: CreateIPSet)
{-# DEPRECATED cisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and underscores (_).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisName :: Lens.Lens' CreateIPSet Lude.Text
cisName = Lens.lens (name :: CreateIPSet -> Lude.Text) (\s a -> s {name = a} :: CreateIPSet)
{-# DEPRECATED cisName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The tags to be added to a new IP set resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisTags :: Lens.Lens' CreateIPSet (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cisTags = Lens.lens (tags :: CreateIPSet -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateIPSet)
{-# DEPRECATED cisTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateIPSet where
  type Rs CreateIPSet = CreateIPSetResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Lude.<$> (x Lude..:> "ipSetId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("location" Lude..= location),
            Lude.Just ("format" Lude..= format),
            Lude.Just ("activate" Lude..= activate),
            Lude.Just ("name" Lude..= name),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateIPSet where
  toPath CreateIPSet' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/ipset"]

instance Lude.ToQuery CreateIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | The ID of the IPSet resource.
    ipSetId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIPSetResponse' with the minimum fields required to make a request.
--
-- * 'ipSetId' - The ID of the IPSet resource.
-- * 'responseStatus' - The response status code.
mkCreateIPSetResponse ::
  -- | 'ipSetId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateIPSetResponse
mkCreateIPSetResponse pIPSetId_ pResponseStatus_ =
  CreateIPSetResponse'
    { ipSetId = pIPSetId_,
      responseStatus = pResponseStatus_
    }

-- | The ID of the IPSet resource.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsIPSetId :: Lens.Lens' CreateIPSetResponse Lude.Text
cisrsIPSetId = Lens.lens (ipSetId :: CreateIPSetResponse -> Lude.Text) (\s a -> s {ipSetId = a} :: CreateIPSetResponse)
{-# DEPRECATED cisrsIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsResponseStatus :: Lens.Lens' CreateIPSetResponse Lude.Int
cisrsResponseStatus = Lens.lens (responseStatus :: CreateIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIPSetResponse)
{-# DEPRECATED cisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
