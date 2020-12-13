{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeConditionalForwarders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the conditional forwarders for this account.
--
-- If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.
module Network.AWS.DirectoryService.DescribeConditionalForwarders
  ( -- * Creating a request
    DescribeConditionalForwarders (..),
    mkDescribeConditionalForwarders,

    -- ** Request lenses
    dcfsDirectoryId,
    dcfsRemoteDomainNames,

    -- * Destructuring the response
    DescribeConditionalForwardersResponse (..),
    mkDescribeConditionalForwardersResponse,

    -- ** Response lenses
    dcfsrsConditionalForwarders,
    dcfsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes a conditional forwarder.
--
-- /See:/ 'mkDescribeConditionalForwarders' smart constructor.
data DescribeConditionalForwarders = DescribeConditionalForwarders'
  { -- | The directory ID for which to get the list of associated conditional forwarders.
    directoryId :: Lude.Text,
    -- | The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
    remoteDomainNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConditionalForwarders' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory ID for which to get the list of associated conditional forwarders.
-- * 'remoteDomainNames' - The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
mkDescribeConditionalForwarders ::
  -- | 'directoryId'
  Lude.Text ->
  DescribeConditionalForwarders
mkDescribeConditionalForwarders pDirectoryId_ =
  DescribeConditionalForwarders'
    { directoryId = pDirectoryId_,
      remoteDomainNames = Lude.Nothing
    }

-- | The directory ID for which to get the list of associated conditional forwarders.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfsDirectoryId :: Lens.Lens' DescribeConditionalForwarders Lude.Text
dcfsDirectoryId = Lens.lens (directoryId :: DescribeConditionalForwarders -> Lude.Text) (\s a -> s {directoryId = a} :: DescribeConditionalForwarders)
{-# DEPRECATED dcfsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
--
-- /Note:/ Consider using 'remoteDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfsRemoteDomainNames :: Lens.Lens' DescribeConditionalForwarders (Lude.Maybe [Lude.Text])
dcfsRemoteDomainNames = Lens.lens (remoteDomainNames :: DescribeConditionalForwarders -> Lude.Maybe [Lude.Text]) (\s a -> s {remoteDomainNames = a} :: DescribeConditionalForwarders)
{-# DEPRECATED dcfsRemoteDomainNames "Use generic-lens or generic-optics with 'remoteDomainNames' instead." #-}

instance Lude.AWSRequest DescribeConditionalForwarders where
  type
    Rs DescribeConditionalForwarders =
      DescribeConditionalForwardersResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConditionalForwardersResponse'
            Lude.<$> (x Lude..?> "ConditionalForwarders" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConditionalForwarders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeConditionalForwarders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConditionalForwarders where
  toJSON DescribeConditionalForwarders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            ("RemoteDomainNames" Lude..=) Lude.<$> remoteDomainNames
          ]
      )

instance Lude.ToPath DescribeConditionalForwarders where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConditionalForwarders where
  toQuery = Lude.const Lude.mempty

-- | The result of a DescribeConditionalForwarder request.
--
-- /See:/ 'mkDescribeConditionalForwardersResponse' smart constructor.
data DescribeConditionalForwardersResponse = DescribeConditionalForwardersResponse'
  { -- | The list of conditional forwarders that have been created.
    conditionalForwarders :: Lude.Maybe [ConditionalForwarder],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConditionalForwardersResponse' with the minimum fields required to make a request.
--
-- * 'conditionalForwarders' - The list of conditional forwarders that have been created.
-- * 'responseStatus' - The response status code.
mkDescribeConditionalForwardersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConditionalForwardersResponse
mkDescribeConditionalForwardersResponse pResponseStatus_ =
  DescribeConditionalForwardersResponse'
    { conditionalForwarders =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of conditional forwarders that have been created.
--
-- /Note:/ Consider using 'conditionalForwarders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfsrsConditionalForwarders :: Lens.Lens' DescribeConditionalForwardersResponse (Lude.Maybe [ConditionalForwarder])
dcfsrsConditionalForwarders = Lens.lens (conditionalForwarders :: DescribeConditionalForwardersResponse -> Lude.Maybe [ConditionalForwarder]) (\s a -> s {conditionalForwarders = a} :: DescribeConditionalForwardersResponse)
{-# DEPRECATED dcfsrsConditionalForwarders "Use generic-lens or generic-optics with 'conditionalForwarders' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfsrsResponseStatus :: Lens.Lens' DescribeConditionalForwardersResponse Lude.Int
dcfsrsResponseStatus = Lens.lens (responseStatus :: DescribeConditionalForwardersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConditionalForwardersResponse)
{-# DEPRECATED dcfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
