{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A partner event source is created by an SaaS partner. If a customer creates a partner event bus that matches this event source, that AWS account can receive events from the partner's applications or services.
--
--
--
-- /See:/ 'partnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { _pesARN ::
      !(Maybe Text),
    _pesName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartnerEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesARN' - The ARN of the partner event source.
--
-- * 'pesName' - The name of the partner event source.
partnerEventSource ::
  PartnerEventSource
partnerEventSource =
  PartnerEventSource' {_pesARN = Nothing, _pesName = Nothing}

-- | The ARN of the partner event source.
pesARN :: Lens' PartnerEventSource (Maybe Text)
pesARN = lens _pesARN (\s a -> s {_pesARN = a})

-- | The name of the partner event source.
pesName :: Lens' PartnerEventSource (Maybe Text)
pesName = lens _pesName (\s a -> s {_pesName = a})

instance FromJSON PartnerEventSource where
  parseJSON =
    withObject
      "PartnerEventSource"
      (\x -> PartnerEventSource' <$> (x .:? "Arn") <*> (x .:? "Name"))

instance Hashable PartnerEventSource

instance NFData PartnerEventSource
