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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @GetCheckerIpRanges@ still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html IP Address Ranges of Amazon Route 53 Servers> in the /Amazon Route 53 Developer Guide/ .
--
--
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'getCheckerIPRanges' smart constructor.
data GetCheckerIPRanges =
  GetCheckerIPRanges'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


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

instance Hashable GetCheckerIPRanges where

instance NFData GetCheckerIPRanges where

instance ToHeaders GetCheckerIPRanges where
        toHeaders = const mempty

instance ToPath GetCheckerIPRanges where
        toPath = const "/2013-04-01/checkeripranges"

instance ToQuery GetCheckerIPRanges where
        toQuery = const mempty

-- | /See:/ 'getCheckerIPRangesResponse' smart constructor.
data GetCheckerIPRangesResponse = GetCheckerIPRangesResponse'
  { _gcirrsResponseStatus  :: !Int
  , _gcirrsCheckerIPRanges :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCheckerIPRangesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirrsResponseStatus' - -- | The response status code.
--
-- * 'gcirrsCheckerIPRanges' - Undocumented member.
getCheckerIPRangesResponse
    :: Int -- ^ 'gcirrsResponseStatus'
    -> GetCheckerIPRangesResponse
getCheckerIPRangesResponse pResponseStatus_ =
  GetCheckerIPRangesResponse'
    {_gcirrsResponseStatus = pResponseStatus_, _gcirrsCheckerIPRanges = mempty}


-- | -- | The response status code.
gcirrsResponseStatus :: Lens' GetCheckerIPRangesResponse Int
gcirrsResponseStatus = lens _gcirrsResponseStatus (\ s a -> s{_gcirrsResponseStatus = a})

-- | Undocumented member.
gcirrsCheckerIPRanges :: Lens' GetCheckerIPRangesResponse [Text]
gcirrsCheckerIPRanges = lens _gcirrsCheckerIPRanges (\ s a -> s{_gcirrsCheckerIPRanges = a}) . _Coerce

instance NFData GetCheckerIPRangesResponse where
