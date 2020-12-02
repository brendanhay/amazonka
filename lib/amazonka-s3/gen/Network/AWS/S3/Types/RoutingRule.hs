{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RoutingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RoutingRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.Redirect

-- | Specifies the redirect behavior and when a redirect is applied. For more information about routing rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'routingRule' smart constructor.
data RoutingRule = RoutingRule'
  { _rrCondition :: !(Maybe Condition),
    _rrRedirect :: !Redirect
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrCondition' - A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- * 'rrRedirect' - Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
routingRule ::
  -- | 'rrRedirect'
  Redirect ->
  RoutingRule
routingRule pRedirect_ =
  RoutingRule' {_rrCondition = Nothing, _rrRedirect = pRedirect_}

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\s a -> s {_rrCondition = a})

-- | Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\s a -> s {_rrRedirect = a})

instance FromXML RoutingRule where
  parseXML x =
    RoutingRule' <$> (x .@? "Condition") <*> (x .@ "Redirect")

instance Hashable RoutingRule

instance NFData RoutingRule

instance ToXML RoutingRule where
  toXML RoutingRule' {..} =
    mconcat ["Condition" @= _rrCondition, "Redirect" @= _rrRedirect]
