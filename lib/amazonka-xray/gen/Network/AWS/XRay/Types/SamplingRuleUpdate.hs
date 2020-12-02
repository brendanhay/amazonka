{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A document specifying changes to a sampling rule's configuration.
--
--
--
-- /See:/ 'samplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { _sruHTTPMethod ::
      !(Maybe Text),
    _sruPriority :: !(Maybe Int),
    _sruRuleName :: !(Maybe Text),
    _sruReservoirSize :: !(Maybe Int),
    _sruFixedRate :: !(Maybe Double),
    _sruResourceARN :: !(Maybe Text),
    _sruAttributes :: !(Maybe (Map Text (Text))),
    _sruServiceName :: !(Maybe Text),
    _sruServiceType :: !(Maybe Text),
    _sruHost :: !(Maybe Text),
    _sruRuleARN :: !(Maybe Text),
    _sruURLPath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SamplingRuleUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sruHTTPMethod' - Matches the HTTP method of a request.
--
-- * 'sruPriority' - The priority of the sampling rule.
--
-- * 'sruRuleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'sruReservoirSize' - A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- * 'sruFixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- * 'sruResourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- * 'sruAttributes' - Matches attributes derived from the request.
--
-- * 'sruServiceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- * 'sruServiceType' - Matches the @origin@ that the service uses to identify its type in segments.
--
-- * 'sruHost' - Matches the hostname from a request URL.
--
-- * 'sruRuleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'sruURLPath' - Matches the path from a request URL.
samplingRuleUpdate ::
  SamplingRuleUpdate
samplingRuleUpdate =
  SamplingRuleUpdate'
    { _sruHTTPMethod = Nothing,
      _sruPriority = Nothing,
      _sruRuleName = Nothing,
      _sruReservoirSize = Nothing,
      _sruFixedRate = Nothing,
      _sruResourceARN = Nothing,
      _sruAttributes = Nothing,
      _sruServiceName = Nothing,
      _sruServiceType = Nothing,
      _sruHost = Nothing,
      _sruRuleARN = Nothing,
      _sruURLPath = Nothing
    }

-- | Matches the HTTP method of a request.
sruHTTPMethod :: Lens' SamplingRuleUpdate (Maybe Text)
sruHTTPMethod = lens _sruHTTPMethod (\s a -> s {_sruHTTPMethod = a})

-- | The priority of the sampling rule.
sruPriority :: Lens' SamplingRuleUpdate (Maybe Int)
sruPriority = lens _sruPriority (\s a -> s {_sruPriority = a})

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
sruRuleName :: Lens' SamplingRuleUpdate (Maybe Text)
sruRuleName = lens _sruRuleName (\s a -> s {_sruRuleName = a})

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
sruReservoirSize :: Lens' SamplingRuleUpdate (Maybe Int)
sruReservoirSize = lens _sruReservoirSize (\s a -> s {_sruReservoirSize = a})

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
sruFixedRate :: Lens' SamplingRuleUpdate (Maybe Double)
sruFixedRate = lens _sruFixedRate (\s a -> s {_sruFixedRate = a})

-- | Matches the ARN of the AWS resource on which the service runs.
sruResourceARN :: Lens' SamplingRuleUpdate (Maybe Text)
sruResourceARN = lens _sruResourceARN (\s a -> s {_sruResourceARN = a})

-- | Matches attributes derived from the request.
sruAttributes :: Lens' SamplingRuleUpdate (HashMap Text (Text))
sruAttributes = lens _sruAttributes (\s a -> s {_sruAttributes = a}) . _Default . _Map

-- | Matches the @name@ that the service uses to identify itself in segments.
sruServiceName :: Lens' SamplingRuleUpdate (Maybe Text)
sruServiceName = lens _sruServiceName (\s a -> s {_sruServiceName = a})

-- | Matches the @origin@ that the service uses to identify its type in segments.
sruServiceType :: Lens' SamplingRuleUpdate (Maybe Text)
sruServiceType = lens _sruServiceType (\s a -> s {_sruServiceType = a})

-- | Matches the hostname from a request URL.
sruHost :: Lens' SamplingRuleUpdate (Maybe Text)
sruHost = lens _sruHost (\s a -> s {_sruHost = a})

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
sruRuleARN :: Lens' SamplingRuleUpdate (Maybe Text)
sruRuleARN = lens _sruRuleARN (\s a -> s {_sruRuleARN = a})

-- | Matches the path from a request URL.
sruURLPath :: Lens' SamplingRuleUpdate (Maybe Text)
sruURLPath = lens _sruURLPath (\s a -> s {_sruURLPath = a})

instance Hashable SamplingRuleUpdate

instance NFData SamplingRuleUpdate

instance ToJSON SamplingRuleUpdate where
  toJSON SamplingRuleUpdate' {..} =
    object
      ( catMaybes
          [ ("HTTPMethod" .=) <$> _sruHTTPMethod,
            ("Priority" .=) <$> _sruPriority,
            ("RuleName" .=) <$> _sruRuleName,
            ("ReservoirSize" .=) <$> _sruReservoirSize,
            ("FixedRate" .=) <$> _sruFixedRate,
            ("ResourceARN" .=) <$> _sruResourceARN,
            ("Attributes" .=) <$> _sruAttributes,
            ("ServiceName" .=) <$> _sruServiceName,
            ("ServiceType" .=) <$> _sruServiceType,
            ("Host" .=) <$> _sruHost,
            ("RuleARN" .=) <$> _sruRuleARN,
            ("URLPath" .=) <$> _sruURLPath
          ]
      )
