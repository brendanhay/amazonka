{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.LoggingConfiguration
  ( LoggingConfiguration (..),

    -- * Smart constructor
    mkLoggingConfiguration,

    -- * Lenses
    lcRedactedFields,
    lcResourceARN,
    lcLogDestinationConfigs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.FieldToMatch

-- | The Amazon Kinesis Data Firehose, @RedactedFields@ information, and the web ACL Amazon Resource Name (ARN).
--
-- /See:/ 'mkLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
    redactedFields :: Lude.Maybe [FieldToMatch],
    -- | The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
    resourceARN :: Lude.Text,
    -- | An array of Amazon Kinesis Data Firehose ARNs.
    logDestinationConfigs :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingConfiguration' with the minimum fields required to make a request.
--
-- * 'redactedFields' - The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
-- * 'logDestinationConfigs' - An array of Amazon Kinesis Data Firehose ARNs.
mkLoggingConfiguration ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'logDestinationConfigs'
  Lude.NonEmpty Lude.Text ->
  LoggingConfiguration
mkLoggingConfiguration pResourceARN_ pLogDestinationConfigs_ =
  LoggingConfiguration'
    { redactedFields = Lude.Nothing,
      resourceARN = pResourceARN_,
      logDestinationConfigs = pLogDestinationConfigs_
    }

-- | The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
--
-- /Note:/ Consider using 'redactedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcRedactedFields :: Lens.Lens' LoggingConfiguration (Lude.Maybe [FieldToMatch])
lcRedactedFields = Lens.lens (redactedFields :: LoggingConfiguration -> Lude.Maybe [FieldToMatch]) (\s a -> s {redactedFields = a} :: LoggingConfiguration)
{-# DEPRECATED lcRedactedFields "Use generic-lens or generic-optics with 'redactedFields' instead." #-}

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcResourceARN :: Lens.Lens' LoggingConfiguration Lude.Text
lcResourceARN = Lens.lens (resourceARN :: LoggingConfiguration -> Lude.Text) (\s a -> s {resourceARN = a} :: LoggingConfiguration)
{-# DEPRECATED lcResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | An array of Amazon Kinesis Data Firehose ARNs.
--
-- /Note:/ Consider using 'logDestinationConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogDestinationConfigs :: Lens.Lens' LoggingConfiguration (Lude.NonEmpty Lude.Text)
lcLogDestinationConfigs = Lens.lens (logDestinationConfigs :: LoggingConfiguration -> Lude.NonEmpty Lude.Text) (\s a -> s {logDestinationConfigs = a} :: LoggingConfiguration)
{-# DEPRECATED lcLogDestinationConfigs "Use generic-lens or generic-optics with 'logDestinationConfigs' instead." #-}

instance Lude.FromJSON LoggingConfiguration where
  parseJSON =
    Lude.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Lude.<$> (x Lude..:? "RedactedFields" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ResourceArn")
            Lude.<*> (x Lude..: "LogDestinationConfigs")
      )

instance Lude.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RedactedFields" Lude..=) Lude.<$> redactedFields,
            Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("LogDestinationConfigs" Lude..= logDestinationConfigs)
          ]
      )
