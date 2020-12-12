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
    chaState,
    chaLogLevel,
    chaARN,
    chaPipelinesRunningCount,
    chaPipelineDetails,
    chaInputSpecification,
    chaInputAttachments,
    chaDestinations,
    chaName,
    chaCdiInputSpecification,
    chaId,
    chaChannelClass,
    chaEgressEndpoints,
    chaTags,
    chaEncoderSettings,
    chaRoleARN,
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
    logLevel :: Lude.Maybe LogLevel,
    arn :: Lude.Maybe Lude.Text,
    pipelinesRunningCount :: Lude.Maybe Lude.Int,
    pipelineDetails :: Lude.Maybe [PipelineDetail],
    inputSpecification :: Lude.Maybe InputSpecification,
    inputAttachments :: Lude.Maybe [InputAttachment],
    destinations :: Lude.Maybe [OutputDestination],
    name :: Lude.Maybe Lude.Text,
    cdiInputSpecification :: Lude.Maybe CdiInputSpecification,
    id :: Lude.Maybe Lude.Text,
    channelClass :: Lude.Maybe ChannelClass,
    egressEndpoints :: Lude.Maybe [ChannelEgressEndpoint],
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encoderSettings :: Lude.Maybe EncoderSettings,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'arn' - The unique arn of the channel.
-- * 'cdiInputSpecification' - Specification of CDI inputs for this channel
-- * 'channelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
-- * 'destinations' - A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
-- * 'egressEndpoints' - The endpoints where outgoing connections initiate from
-- * 'encoderSettings' - Undocumented field.
-- * 'id' - The unique id of the channel.
-- * 'inputAttachments' - List of input attachments for channel.
-- * 'inputSpecification' - Specification of network and file inputs for this channel
-- * 'logLevel' - The log level being written to CloudWatch Logs.
-- * 'name' - The name of the channel. (user-mutable)
-- * 'pipelineDetails' - Runtime details for the pipelines of a running channel.
-- * 'pipelinesRunningCount' - The number of currently healthy pipelines.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
-- * 'state' - Undocumented field.
-- * 'tags' - A collection of key-value pairs.
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
chaState :: Lens.Lens' Channel (Lude.Maybe ChannelState)
chaState = Lens.lens (state :: Channel -> Lude.Maybe ChannelState) (\s a -> s {state = a} :: Channel)
{-# DEPRECATED chaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The log level being written to CloudWatch Logs.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaLogLevel :: Lens.Lens' Channel (Lude.Maybe LogLevel)
chaLogLevel = Lens.lens (logLevel :: Channel -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: Channel)
{-# DEPRECATED chaLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The unique arn of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaARN :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
chaARN = Lens.lens (arn :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Channel)
{-# DEPRECATED chaARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaPipelinesRunningCount :: Lens.Lens' Channel (Lude.Maybe Lude.Int)
chaPipelinesRunningCount = Lens.lens (pipelinesRunningCount :: Channel -> Lude.Maybe Lude.Int) (\s a -> s {pipelinesRunningCount = a} :: Channel)
{-# DEPRECATED chaPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | Runtime details for the pipelines of a running channel.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaPipelineDetails :: Lens.Lens' Channel (Lude.Maybe [PipelineDetail])
chaPipelineDetails = Lens.lens (pipelineDetails :: Channel -> Lude.Maybe [PipelineDetail]) (\s a -> s {pipelineDetails = a} :: Channel)
{-# DEPRECATED chaPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | Specification of network and file inputs for this channel
--
-- /Note:/ Consider using 'inputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaInputSpecification :: Lens.Lens' Channel (Lude.Maybe InputSpecification)
chaInputSpecification = Lens.lens (inputSpecification :: Channel -> Lude.Maybe InputSpecification) (\s a -> s {inputSpecification = a} :: Channel)
{-# DEPRECATED chaInputSpecification "Use generic-lens or generic-optics with 'inputSpecification' instead." #-}

-- | List of input attachments for channel.
--
-- /Note:/ Consider using 'inputAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaInputAttachments :: Lens.Lens' Channel (Lude.Maybe [InputAttachment])
chaInputAttachments = Lens.lens (inputAttachments :: Channel -> Lude.Maybe [InputAttachment]) (\s a -> s {inputAttachments = a} :: Channel)
{-# DEPRECATED chaInputAttachments "Use generic-lens or generic-optics with 'inputAttachments' instead." #-}

-- | A list of destinations of the channel. For UDP outputs, there is one
--
-- destination per output. For other types (HLS, for example), there is
-- one destination per packager.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaDestinations :: Lens.Lens' Channel (Lude.Maybe [OutputDestination])
chaDestinations = Lens.lens (destinations :: Channel -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: Channel)
{-# DEPRECATED chaDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The name of the channel. (user-mutable)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaName :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
chaName = Lens.lens (name :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Channel)
{-# DEPRECATED chaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specification of CDI inputs for this channel
--
-- /Note:/ Consider using 'cdiInputSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaCdiInputSpecification :: Lens.Lens' Channel (Lude.Maybe CdiInputSpecification)
chaCdiInputSpecification = Lens.lens (cdiInputSpecification :: Channel -> Lude.Maybe CdiInputSpecification) (\s a -> s {cdiInputSpecification = a} :: Channel)
{-# DEPRECATED chaCdiInputSpecification "Use generic-lens or generic-optics with 'cdiInputSpecification' instead." #-}

-- | The unique id of the channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaId :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
chaId = Lens.lens (id :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Channel)
{-# DEPRECATED chaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaChannelClass :: Lens.Lens' Channel (Lude.Maybe ChannelClass)
chaChannelClass = Lens.lens (channelClass :: Channel -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: Channel)
{-# DEPRECATED chaChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | The endpoints where outgoing connections initiate from
--
-- /Note:/ Consider using 'egressEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaEgressEndpoints :: Lens.Lens' Channel (Lude.Maybe [ChannelEgressEndpoint])
chaEgressEndpoints = Lens.lens (egressEndpoints :: Channel -> Lude.Maybe [ChannelEgressEndpoint]) (\s a -> s {egressEndpoints = a} :: Channel)
{-# DEPRECATED chaEgressEndpoints "Use generic-lens or generic-optics with 'egressEndpoints' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaTags :: Lens.Lens' Channel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
chaTags = Lens.lens (tags :: Channel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Channel)
{-# DEPRECATED chaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encoderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaEncoderSettings :: Lens.Lens' Channel (Lude.Maybe EncoderSettings)
chaEncoderSettings = Lens.lens (encoderSettings :: Channel -> Lude.Maybe EncoderSettings) (\s a -> s {encoderSettings = a} :: Channel)
{-# DEPRECATED chaEncoderSettings "Use generic-lens or generic-optics with 'encoderSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaRoleARN :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
chaRoleARN = Lens.lens (roleARN :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Channel)
{-# DEPRECATED chaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

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
