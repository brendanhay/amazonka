{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SampledHTTPRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SampledHTTPRequest where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.HTTPRequest

-- | The response from a 'GetSampledRequests' request includes a @SampledHTTPRequests@ complex type that appears as @SampledRequests@ in the response syntax. @SampledHTTPRequests@ contains one @SampledHTTPRequest@ object for each web request that is returned by @GetSampledRequests@ .
--
--
--
-- /See:/ 'sampledHTTPRequest' smart constructor.
data SampledHTTPRequest = SampledHTTPRequest'
  { _shttprRuleWithinRuleGroup ::
      !(Maybe Text),
    _shttprAction :: !(Maybe Text),
    _shttprTimestamp :: !(Maybe POSIX),
    _shttprRequest :: !HTTPRequest,
    _shttprWeight :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SampledHTTPRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shttprRuleWithinRuleGroup' - This value is returned if the @GetSampledRequests@ request specifies the ID of a @RuleGroup@ rather than the ID of an individual rule. @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that matched the request listed in the response.
--
-- * 'shttprAction' - The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
-- * 'shttprTimestamp' - The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
--
-- * 'shttprRequest' - A complex type that contains detailed information about the request.
--
-- * 'shttprWeight' - A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
sampledHTTPRequest ::
  -- | 'shttprRequest'
  HTTPRequest ->
  -- | 'shttprWeight'
  Natural ->
  SampledHTTPRequest
sampledHTTPRequest pRequest_ pWeight_ =
  SampledHTTPRequest'
    { _shttprRuleWithinRuleGroup = Nothing,
      _shttprAction = Nothing,
      _shttprTimestamp = Nothing,
      _shttprRequest = pRequest_,
      _shttprWeight = _Nat # pWeight_
    }

-- | This value is returned if the @GetSampledRequests@ request specifies the ID of a @RuleGroup@ rather than the ID of an individual rule. @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that matched the request listed in the response.
shttprRuleWithinRuleGroup :: Lens' SampledHTTPRequest (Maybe Text)
shttprRuleWithinRuleGroup = lens _shttprRuleWithinRuleGroup (\s a -> s {_shttprRuleWithinRuleGroup = a})

-- | The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
shttprAction :: Lens' SampledHTTPRequest (Maybe Text)
shttprAction = lens _shttprAction (\s a -> s {_shttprAction = a})

-- | The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
shttprTimestamp :: Lens' SampledHTTPRequest (Maybe UTCTime)
shttprTimestamp = lens _shttprTimestamp (\s a -> s {_shttprTimestamp = a}) . mapping _Time

-- | A complex type that contains detailed information about the request.
shttprRequest :: Lens' SampledHTTPRequest HTTPRequest
shttprRequest = lens _shttprRequest (\s a -> s {_shttprRequest = a})

-- | A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
shttprWeight :: Lens' SampledHTTPRequest Natural
shttprWeight = lens _shttprWeight (\s a -> s {_shttprWeight = a}) . _Nat

instance FromJSON SampledHTTPRequest where
  parseJSON =
    withObject
      "SampledHTTPRequest"
      ( \x ->
          SampledHTTPRequest'
            <$> (x .:? "RuleWithinRuleGroup")
            <*> (x .:? "Action")
            <*> (x .:? "Timestamp")
            <*> (x .: "Request")
            <*> (x .: "Weight")
      )

instance Hashable SampledHTTPRequest

instance NFData SampledHTTPRequest
