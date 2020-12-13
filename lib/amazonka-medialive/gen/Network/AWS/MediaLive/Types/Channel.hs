{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cfState,
    cfLogLevel,
    cfARN,
    cfPipelinesRunningCount,
    cfPipelineDetails,
    cfInputSpecification,
    cfInputAttachments,
    cfDestinations,
    cfName,
    cfCdiInputSpecification,
    cfId,
    cfChannelClass,
    cfEgressEndpoints,
    cfTags,
    cfEncoderSettings,
    cfRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.EncoderSettings
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.MediaLive.Types.PipelineDetail
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for Channel
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { state :: Lude.Maybe ChannelState,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Lude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Lude.Maybe [PipelineDetail],
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
    encoderSettings :: Lude.Maybe EncoderSettings,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'state' -
-- * 'logLevel' - The log level being written to CloudWatch Logs.
-- * 'arn' - The unique arn of the channel.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'pipelineDetails' - Runtime details for the pipelines of a running channel.
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
-- * 'encoderSettings' -
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
mkChannel ::
  Channel
mkChannel =
  Channel'
    { state = Lude.Nothing,
      logLevel = Lude.Nothing,
      arn = Lude.Nothing,
      pipelinesRunningCount = Lude.Nothing,
      pipelineDetails = Lude.Nothing,
      inputSpecification = Lude.Nothing,
      inputAttachments = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      cdiInputSpecification = Lude.Nothing,
      id = Lude.Nothing,
      channelClass = Lude.Nothing,
      egressEndpoints = Lude.Nothing,
      tags = Lude.Nothing,
      encoderSettings = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfState :: Lens.Lens' Channel (Lude.Maybe ChannelState)
cfState = Lens.lens (state :: Channel -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: Channel)
{-# DEPRECATED cfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogLevel :: Lens.Lens' Channel (Lude.Maybe LogLevel)
cfLogLevel = Lens.lens (logLevel :: Channel -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: Channel)
{-# DEPRECATED cfLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfARN :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfARN = Lens.lens (arn :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Channel)
{-# DEPRECATED cfARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPipelinesRunningCount :: Lens.Lens' Channel (Lude.Maybe Lude.Int)
cfPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: Channel -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: Channel)
{-# DEPRECATED cfPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPipelineDetails :: Lens.Lens' Channel (Lude.Maybe [PipelineDetail])
cfPipelineDetails = Lens.lens (pipelineDetails :: Channel -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: Channel)
{-# DEPRECATED cfPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInputSpecification :: Lens.Lens' Channel (Lude.Maybe InputSpecification)
cfInputSpecification = Lens.lens (inputSpecification :: Channel -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: Channel)
{-# DEPRECATED cfInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInputAttachments :: Lens.Lens' Channel (Lude.Maybe [InputAttachment])
cfInputAttachments = Lens.lens (inputAttachments :: Channel -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: Channel)
{-# DEPRECATED cfInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDestinations :: Lens.Lens' Channel (Lude.Maybe [OutputDestination])
cfDestinations = Lens.lens (destinations :: Channel -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: Channel)
{-# DEPRECATED cfDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Channel)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCdiInputSpecification :: Lens.Lens' Channel (Lude.Maybe CdiInputSpecification)
cfCdiInputSpecification = Lens.lens (cdiInputSpecification :: Channel -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: Channel)
{-# DEPRECATED cfCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfId = Lens.lens (id :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Channel)
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfChannelClass :: Lens.Lens' Channel (Lude.Maybe ChannelClass)
cfChannelClass = Lens.lens (channelClass :: Channel -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: Channel)
{-# DEPRECATED cfChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEgressEndpoints :: Lens.Lens' Channel (Lude.Maybe [ChannelEgressEndpoint])
cfEgressEndpoints = Lens.lens (egressEndpoints :: Channel -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: Channel)
{-# DEPRECATED cfEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' Channel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfTags = Lens.lens (tags :: Channel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Channel)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEncoderSettings :: Lens.Lens' Channel (Lude.Maybe EncoderSettings)
cfEncoderSettings = Lens.lens (encoderSettings :: Channel -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: Channel)
{-# DEPRECATED cfEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRoleARN :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfRoleARN = Lens.lens (roleARN :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Channel)
{-# DEPRECATED cfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON Channel where
  parseJSON =
    Lude.withObject
      "Channel"
      ( \x ->
          Channel'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "logLevel")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "pipelinesRunningCount")
            Lude.<*> (x Lude..:? "pipelineDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inputSpecification")
            Lude.<*> (x Lude..:? "inputAttachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "cdiInputSpecification")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "channelClass")
            Lude.<*> (x Lude..:? "egressEndpoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "encoderSettings")
            Lude.<*> (x Lude..:? "roleArn")
      )
