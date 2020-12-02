{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type of the configuration to override the risk decision.
--
--
--
-- /See:/ 'riskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { _rectSkippedIPRangeList ::
      !(Maybe [Text]),
    _rectBlockedIPRangeList ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RiskExceptionConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rectSkippedIPRangeList' - Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
--
-- * 'rectBlockedIPRangeList' - Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
riskExceptionConfigurationType ::
  RiskExceptionConfigurationType
riskExceptionConfigurationType =
  RiskExceptionConfigurationType'
    { _rectSkippedIPRangeList =
        Nothing,
      _rectBlockedIPRangeList = Nothing
    }

-- | Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
rectSkippedIPRangeList :: Lens' RiskExceptionConfigurationType [Text]
rectSkippedIPRangeList = lens _rectSkippedIPRangeList (\s a -> s {_rectSkippedIPRangeList = a}) . _Default . _Coerce

-- | Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
rectBlockedIPRangeList :: Lens' RiskExceptionConfigurationType [Text]
rectBlockedIPRangeList = lens _rectBlockedIPRangeList (\s a -> s {_rectBlockedIPRangeList = a}) . _Default . _Coerce

instance FromJSON RiskExceptionConfigurationType where
  parseJSON =
    withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            <$> (x .:? "SkippedIPRangeList" .!= mempty)
            <*> (x .:? "BlockedIPRangeList" .!= mempty)
      )

instance Hashable RiskExceptionConfigurationType

instance NFData RiskExceptionConfigurationType

instance ToJSON RiskExceptionConfigurationType where
  toJSON RiskExceptionConfigurationType' {..} =
    object
      ( catMaybes
          [ ("SkippedIPRangeList" .=) <$> _rectSkippedIPRangeList,
            ("BlockedIPRangeList" .=) <$> _rectBlockedIPRangeList
          ]
      )
