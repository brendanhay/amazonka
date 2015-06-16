{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Support.DescribeSeverityLevels
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the list of severity levels that you can assign to an AWS
-- Support case. The severity level for a case is also a field in the
-- CaseDetails data type included in any CreateCase request.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeSeverityLevels.html>
module Network.AWS.Support.DescribeSeverityLevels
    (
    -- * Request
      DescribeSeverityLevels
    -- ** Request constructor
    , describeSeverityLevels
    -- ** Request lenses
    , dslLanguage

    -- * Response
    , DescribeSeverityLevelsResponse
    -- ** Response constructor
    , describeSeverityLevelsResponse
    -- ** Response lenses
    , dslrSeverityLevels
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Support.Types

-- | /See:/ 'describeSeverityLevels' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dslLanguage'
newtype DescribeSeverityLevels = DescribeSeverityLevels'{_dslLanguage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeSeverityLevels' smart constructor.
describeSeverityLevels :: DescribeSeverityLevels
describeSeverityLevels = DescribeSeverityLevels'{_dslLanguage = Nothing};

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dslLanguage :: Lens' DescribeSeverityLevels (Maybe Text)
dslLanguage = lens _dslLanguage (\ s a -> s{_dslLanguage = a});

instance AWSRequest DescribeSeverityLevels where
        type Sv DescribeSeverityLevels = Support
        type Rs DescribeSeverityLevels =
             DescribeSeverityLevelsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSeverityLevelsResponse' <$>
                   (x .?> "severityLevels" .!@ mempty))

instance ToHeaders DescribeSeverityLevels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeSeverityLevels" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSeverityLevels where
        toJSON DescribeSeverityLevels'{..}
          = object ["language" .= _dslLanguage]

instance ToPath DescribeSeverityLevels where
        toPath = const "/"

instance ToQuery DescribeSeverityLevels where
        toQuery = const mempty

-- | /See:/ 'describeSeverityLevelsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dslrSeverityLevels'
newtype DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse'{_dslrSeverityLevels :: Maybe [SeverityLevel]} deriving (Eq, Read, Show)

-- | 'DescribeSeverityLevelsResponse' smart constructor.
describeSeverityLevelsResponse :: DescribeSeverityLevelsResponse
describeSeverityLevelsResponse = DescribeSeverityLevelsResponse'{_dslrSeverityLevels = Nothing};

-- | The available severity levels for the support case. Available severity
-- levels are defined by your service level agreement with AWS.
dslrSeverityLevels :: Lens' DescribeSeverityLevelsResponse [SeverityLevel]
dslrSeverityLevels = lens _dslrSeverityLevels (\ s a -> s{_dslrSeverityLevels = a}) . _Default;
