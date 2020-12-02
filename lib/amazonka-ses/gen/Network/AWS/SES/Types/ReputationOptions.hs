{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReputationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReputationOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the reputation settings for a configuration set.
--
--
--
-- /See:/ 'reputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { _roLastFreshStart ::
      !(Maybe ISO8601),
    _roReputationMetricsEnabled :: !(Maybe Bool),
    _roSendingEnabled :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReputationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roLastFreshStart' - The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ . When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset. If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
--
-- * 'roReputationMetricsEnabled' - Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch. If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
--
-- * 'roSendingEnabled' - Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
reputationOptions ::
  ReputationOptions
reputationOptions =
  ReputationOptions'
    { _roLastFreshStart = Nothing,
      _roReputationMetricsEnabled = Nothing,
      _roSendingEnabled = Nothing
    }

-- | The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ . When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset. If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
roLastFreshStart :: Lens' ReputationOptions (Maybe UTCTime)
roLastFreshStart = lens _roLastFreshStart (\s a -> s {_roLastFreshStart = a}) . mapping _Time

-- | Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch. If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
roReputationMetricsEnabled :: Lens' ReputationOptions (Maybe Bool)
roReputationMetricsEnabled = lens _roReputationMetricsEnabled (\s a -> s {_roReputationMetricsEnabled = a})

-- | Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
roSendingEnabled :: Lens' ReputationOptions (Maybe Bool)
roSendingEnabled = lens _roSendingEnabled (\s a -> s {_roSendingEnabled = a})

instance FromXML ReputationOptions where
  parseXML x =
    ReputationOptions'
      <$> (x .@? "LastFreshStart")
      <*> (x .@? "ReputationMetricsEnabled")
      <*> (x .@? "SendingEnabled")

instance Hashable ReputationOptions

instance NFData ReputationOptions
