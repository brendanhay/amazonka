{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelSummary
  ( ChannelSummary (..),

    -- * Smart constructor
    mkChannelSummary,

    -- * Lenses
    csState,
    csLogLevel,
    csARN,
    csPipelinesRunningCount,
    csInputSpecification,
    csInputAttachments,
    csDestinations,
    csName,
    csCdiInputSpecification,
    csId,
    csChannelClass,
    csEgressEndpoints,
    csTags,
    csRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'mkChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { state :: Lude.Maybe ChannelState,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Lude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Lude.Maybe InputSpecification,
    -- | List of input attachments for channel.
    inputAttachments :: Lude.Maybe [InputAttachment],
    -- | A list of destinations of the channel. For UDP outputs, there is one
    --
    -- destination per output. For other types (HLS, for example), there is
    -- one destination per packager.
    destinations :: Lude.Maybe [OutputDestination],
    -- | The name of the channel. (user-mutable)
    name :: Lude.Maybe Lude.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Lude.Maybe CdiInputSpecification,
    -- | The unique id of the channel.
    id :: Lude.Maybe Lude.Text,
    -- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Lude.Maybe ChannelClass,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Lude.Maybe [ChannelEgressEndpoint],
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- * 'state' -
-- * 'logLevel' - The log level being written to CloudWatch Logs.
-- * 'arn' - The unique arn of the channel.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'inputAttachments' - List of input attachments for channel.
-- * 'destinations' - A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
-- * 'name' - The name of the channel. (user-mutable)
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'id' - The unique id of the channel.
-- * 'channelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
-- * 'egressEndpoints' - The endpoints where outgoing connections initiate from
-- * 'tags' - A collection of key-value pairs.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
mkChannelSummary ::
  ChannelSummary
mkChannelSummary =
  ChannelSummary'
    { state = Lude.Nothing,
      logLevel = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      inputSpecification = Lude.Nothing,
      inputAttachments = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      cdiInputSpecification = Lude.Nothing,
      id = Lude.Nothing,
      channelClass = Lude.Nothing,
      egressEndpoints = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csState :: Lens.Lens' ChannelSummary (Lude.Maybe ChannelState)
csState = Lens.lens (state :: ChannelSummary -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: ChannelSummary)
{-# DEPRECATED csState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLogLevel :: Lens.Lens' ChannelSummary (Lude.Maybe LogLevel)
csLogLevel = Lens.lens (logLevel :: ChannelSummary -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: ChannelSummary)
{-# DEPRECATED csLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csARN :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Text)
csARN = Lens.lens (arn :: ChannelSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ChannelSummary)
{-# DEPRECATED csARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPipelinesRunningCount :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Int)
csPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: ChannelSummary -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: ChannelSummary)
{-# DEPRECATED csPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInputSpecification :: Lens.Lens' ChannelSummary (Lude.Maybe InputSpecification)
csInputSpecification = Lens.lens (inputSpecification :: ChannelSummary -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: ChannelSummary)
{-# DEPRECATED csInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInputAttachments :: Lens.Lens' ChannelSummary (Lude.Maybe [InputAttachment])
csInputAttachments = Lens.lens (inputAttachments :: ChannelSummary -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: ChannelSummary)
{-# DEPRECATED csInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDestinations :: Lens.Lens' ChannelSummary (Lude.Maybe [OutputDestination])
csDestinations = Lens.lens (destinations :: ChannelSummary -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: ChannelSummary)
{-# DEPRECATED csDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Text)
csName = Lens.lens (name :: ChannelSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ChannelSummary)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCdiInputSpecification :: Lens.Lens' ChannelSummary (Lude.Maybe CdiInputSpecification)
csCdiInputSpecification = Lens.lens (cdiInputSpecification :: ChannelSummary -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: ChannelSummary)
{-# DEPRECATED csCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csId :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Text)
csId = Lens.lens (id :: ChannelSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ChannelSummary)
{-# DEPRECATED csId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChannelClass :: Lens.Lens' ChannelSummary (Lude.Maybe ChannelClass)
csChannelClass = Lens.lens (channelClass :: ChannelSummary -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: ChannelSummary)
{-# DEPRECATED csChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEgressEndpoints :: Lens.Lens' ChannelSummary (Lude.Maybe [ChannelEgressEndpoint])
csEgressEndpoints = Lens.lens (egressEndpoints :: ChannelSummary -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: ChannelSummary)
{-# DEPRECATED csEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' ChannelSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csTags = Lens.lens (tags :: ChannelSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ChannelSummary)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Text)
csRoleARN = Lens.lens (roleARN :: ChannelSummary -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ChannelSummary)
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ChannelSummary where
  parseJSON =
    Lude.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "logLevel")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "pipelinesRunningCount")
            Lude.<*> (x Lude..:? "inputSpecification")
            Lude.<*> (x Lude..:? "inputAttachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "cdiInputSpecification")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "channelClass")
            Lude.<*> (x Lude..:? "egressEndpoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
      )
