{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetCheckerIPRanges
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of the IP ranges used by Amazon Route 53 health
-- checkers to check the health of your resources, send a 'GET' request to
-- the '2013-04-01\/checkeripranges' resource. You can use these IP
-- addresses to configure router and firewall rules to allow health
-- checkers to check the health of your resources.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetCheckerIPRanges.html AWS API Reference> for GetCheckerIPRanges.
module Network.AWS.Route53.GetCheckerIPRanges
    (
    -- * Creating a Request
      getCheckerIPRanges
    , GetCheckerIPRanges

    -- * Destructuring the Response
    , getCheckerIPRangesResponse
    , GetCheckerIPRangesResponse
    -- * Response Lenses
    , gcirrsResponseStatus
    , gcirrsCheckerIPRanges
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | Empty request.
--
-- /See:/ 'getCheckerIPRanges' smart constructor.
data GetCheckerIPRanges =
    GetCheckerIPRanges'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCheckerIPRanges' with the minimum fields required to make a request.
--
getCheckerIPRanges
    :: GetCheckerIPRanges
getCheckerIPRanges = GetCheckerIPRanges'

instance AWSRequest GetCheckerIPRanges where
        type Rs GetCheckerIPRanges =
             GetCheckerIPRangesResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetCheckerIPRangesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "CheckerIpRanges" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders GetCheckerIPRanges where
        toHeaders = const mempty

instance ToPath GetCheckerIPRanges where
        toPath = const "/2013-04-01/checkeripranges"

instance ToQuery GetCheckerIPRanges where
        toQuery = const mempty

-- | A complex type that contains the 'CheckerIpRanges' element.
--
-- /See:/ 'getCheckerIPRangesResponse' smart constructor.
data GetCheckerIPRangesResponse = GetCheckerIPRangesResponse'
    { _gcirrsResponseStatus  :: !Int
    , _gcirrsCheckerIPRanges :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCheckerIPRangesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirrsResponseStatus'
--
-- * 'gcirrsCheckerIPRanges'
getCheckerIPRangesResponse
    :: Int -- ^ 'gcirrsResponseStatus'
    -> GetCheckerIPRangesResponse
getCheckerIPRangesResponse pResponseStatus_ =
    GetCheckerIPRangesResponse'
    { _gcirrsResponseStatus = pResponseStatus_
    , _gcirrsCheckerIPRanges = mempty
    }

-- | The response status code.
gcirrsResponseStatus :: Lens' GetCheckerIPRangesResponse Int
gcirrsResponseStatus = lens _gcirrsResponseStatus (\ s a -> s{_gcirrsResponseStatus = a});

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
gcirrsCheckerIPRanges :: Lens' GetCheckerIPRangesResponse [Text]
gcirrsCheckerIPRanges = lens _gcirrsCheckerIPRanges (\ s a -> s{_gcirrsCheckerIPRanges = a}) . _Coerce;
