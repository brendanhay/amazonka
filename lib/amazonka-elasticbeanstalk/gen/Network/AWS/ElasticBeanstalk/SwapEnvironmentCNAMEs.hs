{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Swaps the CNAMEs of two environments.
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
  ( -- * Creating a request
    SwapEnvironmentCNAMEs (..),
    mkSwapEnvironmentCNAMEs,

    -- ** Request lenses
    secnameDestinationEnvironmentName,
    secnameDestinationEnvironmentId,
    secnameSourceEnvironmentName,
    secnameSourceEnvironmentId,

    -- * Destructuring the response
    SwapEnvironmentCNAMEsResponse (..),
    mkSwapEnvironmentCNAMEsResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Swaps the CNAMEs of two environments.
--
-- /See:/ 'mkSwapEnvironmentCNAMEs' smart constructor.
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'
  { destinationEnvironmentName ::
      Lude.Maybe Lude.Text,
    destinationEnvironmentId ::
      Lude.Maybe Lude.Text,
    sourceEnvironmentName :: Lude.Maybe Lude.Text,
    sourceEnvironmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SwapEnvironmentCNAMEs' with the minimum fields required to make a request.
--
-- * 'destinationEnvironmentId' - The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
-- * 'destinationEnvironmentName' - The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
-- * 'sourceEnvironmentId' - The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
-- * 'sourceEnvironmentName' - The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
mkSwapEnvironmentCNAMEs ::
  SwapEnvironmentCNAMEs
mkSwapEnvironmentCNAMEs =
  SwapEnvironmentCNAMEs'
    { destinationEnvironmentName = Lude.Nothing,
      destinationEnvironmentId = Lude.Nothing,
      sourceEnvironmentName = Lude.Nothing,
      sourceEnvironmentId = Lude.Nothing
    }

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
--
-- /Note:/ Consider using 'destinationEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameDestinationEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Lude.Maybe Lude.Text)
secnameDestinationEnvironmentName = Lens.lens (destinationEnvironmentName :: SwapEnvironmentCNAMEs -> Lude.Maybe Lude.Text) (\s a -> s {destinationEnvironmentName = a} :: SwapEnvironmentCNAMEs)
{-# DEPRECATED secnameDestinationEnvironmentName "Use generic-lens or generic-optics with 'destinationEnvironmentName' instead." #-}

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
--
-- /Note:/ Consider using 'destinationEnvironmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameDestinationEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Lude.Maybe Lude.Text)
secnameDestinationEnvironmentId = Lens.lens (destinationEnvironmentId :: SwapEnvironmentCNAMEs -> Lude.Maybe Lude.Text) (\s a -> s {destinationEnvironmentId = a} :: SwapEnvironmentCNAMEs)
{-# DEPRECATED secnameDestinationEnvironmentId "Use generic-lens or generic-optics with 'destinationEnvironmentId' instead." #-}

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
--
-- /Note:/ Consider using 'sourceEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameSourceEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Lude.Maybe Lude.Text)
secnameSourceEnvironmentName = Lens.lens (sourceEnvironmentName :: SwapEnvironmentCNAMEs -> Lude.Maybe Lude.Text) (\s a -> s {sourceEnvironmentName = a} :: SwapEnvironmentCNAMEs)
{-# DEPRECATED secnameSourceEnvironmentName "Use generic-lens or generic-optics with 'sourceEnvironmentName' instead." #-}

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
--
-- /Note:/ Consider using 'sourceEnvironmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameSourceEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Lude.Maybe Lude.Text)
secnameSourceEnvironmentId = Lens.lens (sourceEnvironmentId :: SwapEnvironmentCNAMEs -> Lude.Maybe Lude.Text) (\s a -> s {sourceEnvironmentId = a} :: SwapEnvironmentCNAMEs)
{-# DEPRECATED secnameSourceEnvironmentId "Use generic-lens or generic-optics with 'sourceEnvironmentId' instead." #-}

instance Lude.AWSRequest SwapEnvironmentCNAMEs where
  type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull SwapEnvironmentCNAMEsResponse'

instance Lude.ToHeaders SwapEnvironmentCNAMEs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SwapEnvironmentCNAMEs where
  toPath = Lude.const "/"

instance Lude.ToQuery SwapEnvironmentCNAMEs where
  toQuery SwapEnvironmentCNAMEs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SwapEnvironmentCNAMEs" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "DestinationEnvironmentName" Lude.=: destinationEnvironmentName,
        "DestinationEnvironmentId" Lude.=: destinationEnvironmentId,
        "SourceEnvironmentName" Lude.=: sourceEnvironmentName,
        "SourceEnvironmentId" Lude.=: sourceEnvironmentId
      ]

-- | /See:/ 'mkSwapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SwapEnvironmentCNAMEsResponse' with the minimum fields required to make a request.
mkSwapEnvironmentCNAMEsResponse ::
  SwapEnvironmentCNAMEsResponse
mkSwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
