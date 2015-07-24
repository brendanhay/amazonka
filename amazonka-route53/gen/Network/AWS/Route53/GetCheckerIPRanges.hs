{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetCheckerIPRanges
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of the IP ranges used by Amazon Route 53 health
-- checkers to check the health of your resources, send a @GET@ request to
-- the @2013-04-01\/checkeripranges@ resource. You can use these IP
-- addresses to configure router and firewall rules to allow health
-- checkers to check the health of your resources.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetCheckerIPRanges.html>
module Network.AWS.Route53.GetCheckerIPRanges
    (
    -- * Request
      GetCheckerIPRanges
    -- ** Request constructor
    , getCheckerIPRanges

    -- * Response
    , GetCheckerIPRangesResponse
    -- ** Response constructor
    , getCheckerIPRangesResponse
    -- ** Response lenses
    , gcirrsStatus
    , gcirrsCheckerIPRanges
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | Empty request.
--
-- /See:/ 'getCheckerIPRanges' smart constructor.
data GetCheckerIPRanges =
    GetCheckerIPRanges'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCheckerIPRanges' smart constructor.
getCheckerIPRanges :: GetCheckerIPRanges
getCheckerIPRanges = GetCheckerIPRanges'

instance AWSRequest GetCheckerIPRanges where
        type Sv GetCheckerIPRanges = Route53
        type Rs GetCheckerIPRanges =
             GetCheckerIPRangesResponse
        request = get "GetCheckerIPRanges"
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

-- | A complex type that contains the @CheckerIpRanges@ element.
--
-- /See:/ 'getCheckerIPRangesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcirrsStatus'
--
-- * 'gcirrsCheckerIPRanges'
data GetCheckerIPRangesResponse = GetCheckerIPRangesResponse'
    { _gcirrsStatus          :: !Int
    , _gcirrsCheckerIPRanges :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCheckerIPRangesResponse' smart constructor.
getCheckerIPRangesResponse :: Int -> GetCheckerIPRangesResponse
getCheckerIPRangesResponse pStatus_ =
    GetCheckerIPRangesResponse'
    { _gcirrsStatus = pStatus_
    , _gcirrsCheckerIPRanges = mempty
    }

-- | FIXME: Undocumented member.
gcirrsStatus :: Lens' GetCheckerIPRangesResponse Int
gcirrsStatus = lens _gcirrsStatus (\ s a -> s{_gcirrsStatus = a});

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
gcirrsCheckerIPRanges :: Lens' GetCheckerIPRangesResponse [Text]
gcirrsCheckerIPRanges = lens _gcirrsCheckerIPRanges (\ s a -> s{_gcirrsCheckerIPRanges = a});
