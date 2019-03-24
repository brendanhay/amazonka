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
-- Module      : Network.AWS.XRay.GetSamplingTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a sampling quota for rules that the service is using to sample requests. 
--
--
module Network.AWS.XRay.GetSamplingTargets
    (
    -- * Creating a Request
      getSamplingTargets
    , GetSamplingTargets
    -- * Request Lenses
    , gstSamplingStatisticsDocuments

    -- * Destructuring the Response
    , getSamplingTargetsResponse
    , GetSamplingTargetsResponse
    -- * Response Lenses
    , gstrsUnprocessedStatistics
    , gstrsLastRuleModification
    , gstrsSamplingTargetDocuments
    , gstrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getSamplingTargets' smart constructor.
newtype GetSamplingTargets = GetSamplingTargets'
  { _gstSamplingStatisticsDocuments :: [SamplingStatisticsDocument]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstSamplingStatisticsDocuments' - Information about rules that the service is using to sample requests.
getSamplingTargets
    :: GetSamplingTargets
getSamplingTargets =
  GetSamplingTargets' {_gstSamplingStatisticsDocuments = mempty}


-- | Information about rules that the service is using to sample requests.
gstSamplingStatisticsDocuments :: Lens' GetSamplingTargets [SamplingStatisticsDocument]
gstSamplingStatisticsDocuments = lens _gstSamplingStatisticsDocuments (\ s a -> s{_gstSamplingStatisticsDocuments = a}) . _Coerce

instance AWSRequest GetSamplingTargets where
        type Rs GetSamplingTargets =
             GetSamplingTargetsResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetSamplingTargetsResponse' <$>
                   (x .?> "UnprocessedStatistics" .!@ mempty) <*>
                     (x .?> "LastRuleModification")
                     <*> (x .?> "SamplingTargetDocuments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetSamplingTargets where

instance NFData GetSamplingTargets where

instance ToHeaders GetSamplingTargets where
        toHeaders = const mempty

instance ToJSON GetSamplingTargets where
        toJSON GetSamplingTargets'{..}
          = object
              (catMaybes
                 [Just
                    ("SamplingStatisticsDocuments" .=
                       _gstSamplingStatisticsDocuments)])

instance ToPath GetSamplingTargets where
        toPath = const "/SamplingTargets"

instance ToQuery GetSamplingTargets where
        toQuery = const mempty

-- | /See:/ 'getSamplingTargetsResponse' smart constructor.
data GetSamplingTargetsResponse = GetSamplingTargetsResponse'
  { _gstrsUnprocessedStatistics :: !(Maybe [UnprocessedStatistics])
  , _gstrsLastRuleModification :: !(Maybe POSIX)
  , _gstrsSamplingTargetDocuments :: !(Maybe [SamplingTargetDocument])
  , _gstrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstrsUnprocessedStatistics' - Information about 'SamplingStatisticsDocument' that X-Ray could not process.
--
-- * 'gstrsLastRuleModification' - The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
--
-- * 'gstrsSamplingTargetDocuments' - Updated rules that the service should use to sample requests.
--
-- * 'gstrsResponseStatus' - -- | The response status code.
getSamplingTargetsResponse
    :: Int -- ^ 'gstrsResponseStatus'
    -> GetSamplingTargetsResponse
getSamplingTargetsResponse pResponseStatus_ =
  GetSamplingTargetsResponse'
    { _gstrsUnprocessedStatistics = Nothing
    , _gstrsLastRuleModification = Nothing
    , _gstrsSamplingTargetDocuments = Nothing
    , _gstrsResponseStatus = pResponseStatus_
    }


-- | Information about 'SamplingStatisticsDocument' that X-Ray could not process.
gstrsUnprocessedStatistics :: Lens' GetSamplingTargetsResponse [UnprocessedStatistics]
gstrsUnprocessedStatistics = lens _gstrsUnprocessedStatistics (\ s a -> s{_gstrsUnprocessedStatistics = a}) . _Default . _Coerce

-- | The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
gstrsLastRuleModification :: Lens' GetSamplingTargetsResponse (Maybe UTCTime)
gstrsLastRuleModification = lens _gstrsLastRuleModification (\ s a -> s{_gstrsLastRuleModification = a}) . mapping _Time

-- | Updated rules that the service should use to sample requests.
gstrsSamplingTargetDocuments :: Lens' GetSamplingTargetsResponse [SamplingTargetDocument]
gstrsSamplingTargetDocuments = lens _gstrsSamplingTargetDocuments (\ s a -> s{_gstrsSamplingTargetDocuments = a}) . _Default . _Coerce

-- | -- | The response status code.
gstrsResponseStatus :: Lens' GetSamplingTargetsResponse Int
gstrsResponseStatus = lens _gstrsResponseStatus (\ s a -> s{_gstrsResponseStatus = a})

instance NFData GetSamplingTargetsResponse where
