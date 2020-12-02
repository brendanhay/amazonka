{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A sampling rule that services use to decide whether to instrument a request. Rule fields can match properties of the service, or properties of a request. The service can ignore rules that don't match its properties.
--
--
--
-- /See:/ 'samplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { _srRuleName :: !(Maybe Text),
    _srAttributes :: !(Maybe (Map Text (Text))),
    _srRuleARN :: !(Maybe Text),
    _srResourceARN :: !Text,
    _srPriority :: !Nat,
    _srFixedRate :: !Double,
    _srReservoirSize :: !Nat,
    _srServiceName :: !Text,
    _srServiceType :: !Text,
    _srHost :: !Text,
    _srHTTPMethod :: !Text,
    _srURLPath :: !Text,
    _srVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SamplingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srRuleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'srAttributes' - Matches attributes derived from the request.
--
-- * 'srRuleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- * 'srResourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- * 'srPriority' - The priority of the sampling rule.
--
-- * 'srFixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- * 'srReservoirSize' - A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- * 'srServiceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- * 'srServiceType' - Matches the @origin@ that the service uses to identify its type in segments.
--
-- * 'srHost' - Matches the hostname from a request URL.
--
-- * 'srHTTPMethod' - Matches the HTTP method of a request.
--
-- * 'srURLPath' - Matches the path from a request URL.
--
-- * 'srVersion' - The version of the sampling rule format (@1@ ).
samplingRule ::
  -- | 'srResourceARN'
  Text ->
  -- | 'srPriority'
  Natural ->
  -- | 'srFixedRate'
  Double ->
  -- | 'srReservoirSize'
  Natural ->
  -- | 'srServiceName'
  Text ->
  -- | 'srServiceType'
  Text ->
  -- | 'srHost'
  Text ->
  -- | 'srHTTPMethod'
  Text ->
  -- | 'srURLPath'
  Text ->
  -- | 'srVersion'
  Natural ->
  SamplingRule
samplingRule
  pResourceARN_
  pPriority_
  pFixedRate_
  pReservoirSize_
  pServiceName_
  pServiceType_
  pHost_
  pHTTPMethod_
  pURLPath_
  pVersion_ =
    SamplingRule'
      { _srRuleName = Nothing,
        _srAttributes = Nothing,
        _srRuleARN = Nothing,
        _srResourceARN = pResourceARN_,
        _srPriority = _Nat # pPriority_,
        _srFixedRate = pFixedRate_,
        _srReservoirSize = _Nat # pReservoirSize_,
        _srServiceName = pServiceName_,
        _srServiceType = pServiceType_,
        _srHost = pHost_,
        _srHTTPMethod = pHTTPMethod_,
        _srURLPath = pURLPath_,
        _srVersion = _Nat # pVersion_
      }

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
srRuleName :: Lens' SamplingRule (Maybe Text)
srRuleName = lens _srRuleName (\s a -> s {_srRuleName = a})

-- | Matches attributes derived from the request.
srAttributes :: Lens' SamplingRule (HashMap Text (Text))
srAttributes = lens _srAttributes (\s a -> s {_srAttributes = a}) . _Default . _Map

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
srRuleARN :: Lens' SamplingRule (Maybe Text)
srRuleARN = lens _srRuleARN (\s a -> s {_srRuleARN = a})

-- | Matches the ARN of the AWS resource on which the service runs.
srResourceARN :: Lens' SamplingRule Text
srResourceARN = lens _srResourceARN (\s a -> s {_srResourceARN = a})

-- | The priority of the sampling rule.
srPriority :: Lens' SamplingRule Natural
srPriority = lens _srPriority (\s a -> s {_srPriority = a}) . _Nat

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
srFixedRate :: Lens' SamplingRule Double
srFixedRate = lens _srFixedRate (\s a -> s {_srFixedRate = a})

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
srReservoirSize :: Lens' SamplingRule Natural
srReservoirSize = lens _srReservoirSize (\s a -> s {_srReservoirSize = a}) . _Nat

-- | Matches the @name@ that the service uses to identify itself in segments.
srServiceName :: Lens' SamplingRule Text
srServiceName = lens _srServiceName (\s a -> s {_srServiceName = a})

-- | Matches the @origin@ that the service uses to identify its type in segments.
srServiceType :: Lens' SamplingRule Text
srServiceType = lens _srServiceType (\s a -> s {_srServiceType = a})

-- | Matches the hostname from a request URL.
srHost :: Lens' SamplingRule Text
srHost = lens _srHost (\s a -> s {_srHost = a})

-- | Matches the HTTP method of a request.
srHTTPMethod :: Lens' SamplingRule Text
srHTTPMethod = lens _srHTTPMethod (\s a -> s {_srHTTPMethod = a})

-- | Matches the path from a request URL.
srURLPath :: Lens' SamplingRule Text
srURLPath = lens _srURLPath (\s a -> s {_srURLPath = a})

-- | The version of the sampling rule format (@1@ ).
srVersion :: Lens' SamplingRule Natural
srVersion = lens _srVersion (\s a -> s {_srVersion = a}) . _Nat

instance FromJSON SamplingRule where
  parseJSON =
    withObject
      "SamplingRule"
      ( \x ->
          SamplingRule'
            <$> (x .:? "RuleName")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "RuleARN")
            <*> (x .: "ResourceARN")
            <*> (x .: "Priority")
            <*> (x .: "FixedRate")
            <*> (x .: "ReservoirSize")
            <*> (x .: "ServiceName")
            <*> (x .: "ServiceType")
            <*> (x .: "Host")
            <*> (x .: "HTTPMethod")
            <*> (x .: "URLPath")
            <*> (x .: "Version")
      )

instance Hashable SamplingRule

instance NFData SamplingRule

instance ToJSON SamplingRule where
  toJSON SamplingRule' {..} =
    object
      ( catMaybes
          [ ("RuleName" .=) <$> _srRuleName,
            ("Attributes" .=) <$> _srAttributes,
            ("RuleARN" .=) <$> _srRuleARN,
            Just ("ResourceARN" .= _srResourceARN),
            Just ("Priority" .= _srPriority),
            Just ("FixedRate" .= _srFixedRate),
            Just ("ReservoirSize" .= _srReservoirSize),
            Just ("ServiceName" .= _srServiceName),
            Just ("ServiceType" .= _srServiceType),
            Just ("Host" .= _srHost),
            Just ("HTTPMethod" .= _srHTTPMethod),
            Just ("URLPath" .= _srURLPath),
            Just ("Version" .= _srVersion)
          ]
      )
