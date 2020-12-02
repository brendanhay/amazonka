{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WebACL where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.WafAction

-- | Contains the @Rules@ that identify the requests that you want to allow, block, or count. In a @WebACL@ , you also specify a default action (@ALLOW@ or @BLOCK@ ), and the action for each @Rule@ that you add to a @WebACL@ , for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the @WebACL@ with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one @Rule@ to a @WebACL@ , a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see 'UpdateWebACL' .
--
--
--
-- /See:/ 'webACL' smart constructor.
data WebACL = WebACL'
  { _waMetricName :: !(Maybe Text),
    _waName :: !(Maybe Text),
    _waWebACLARN :: !(Maybe Text),
    _waWebACLId :: !Text,
    _waDefaultAction :: !WafAction,
    _waRules :: ![ActivatedRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waMetricName' - A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
--
-- * 'waName' - A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
--
-- * 'waWebACLARN' - Tha Amazon Resource Name (ARN) of the web ACL.
--
-- * 'waWebACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'waDefaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
--
-- * 'waRules' - An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
webACL ::
  -- | 'waWebACLId'
  Text ->
  -- | 'waDefaultAction'
  WafAction ->
  WebACL
webACL pWebACLId_ pDefaultAction_ =
  WebACL'
    { _waMetricName = Nothing,
      _waName = Nothing,
      _waWebACLARN = Nothing,
      _waWebACLId = pWebACLId_,
      _waDefaultAction = pDefaultAction_,
      _waRules = mempty
    }

-- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
waMetricName :: Lens' WebACL (Maybe Text)
waMetricName = lens _waMetricName (\s a -> s {_waMetricName = a})

-- | A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
waName :: Lens' WebACL (Maybe Text)
waName = lens _waName (\s a -> s {_waName = a})

-- | Tha Amazon Resource Name (ARN) of the web ACL.
waWebACLARN :: Lens' WebACL (Maybe Text)
waWebACLARN = lens _waWebACLARN (\s a -> s {_waWebACLARN = a})

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
waWebACLId :: Lens' WebACL Text
waWebACLId = lens _waWebACLId (\s a -> s {_waWebACLId = a})

-- | The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
waDefaultAction :: Lens' WebACL WafAction
waDefaultAction = lens _waDefaultAction (\s a -> s {_waDefaultAction = a})

-- | An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
waRules :: Lens' WebACL [ActivatedRule]
waRules = lens _waRules (\s a -> s {_waRules = a}) . _Coerce

instance FromJSON WebACL where
  parseJSON =
    withObject
      "WebACL"
      ( \x ->
          WebACL'
            <$> (x .:? "MetricName")
            <*> (x .:? "Name")
            <*> (x .:? "WebACLArn")
            <*> (x .: "WebACLId")
            <*> (x .: "DefaultAction")
            <*> (x .:? "Rules" .!= mempty)
      )

instance Hashable WebACL

instance NFData WebACL
