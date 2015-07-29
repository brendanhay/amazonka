{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all available Trusted Advisor checks,
-- including name, ID, category, description, and metadata. You must
-- specify a language code; English (\"en\") and Japanese (\"ja\") are
-- currently supported. The response contains a
-- TrustedAdvisorCheckDescription for each check.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorChecks.html>
module Network.AWS.Support.DescribeTrustedAdvisorChecks
    (
    -- * Request
      DescribeTrustedAdvisorChecks
    -- ** Request constructor
    , describeTrustedAdvisorChecks
    -- ** Request lenses
    , dtacLanguage

    -- * Response
    , DescribeTrustedAdvisorChecksResponse
    -- ** Response constructor
    , describeTrustedAdvisorChecksResponse
    -- ** Response lenses
    , dtacrsStatus
    , dtacrsChecks
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeTrustedAdvisorChecks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacLanguage'
newtype DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
    { _dtacLanguage :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorChecks' smart constructor.
describeTrustedAdvisorChecks :: Text -> DescribeTrustedAdvisorChecks
describeTrustedAdvisorChecks pLanguage_ =
    DescribeTrustedAdvisorChecks'
    { _dtacLanguage = pLanguage_
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dtacLanguage :: Lens' DescribeTrustedAdvisorChecks Text
dtacLanguage = lens _dtacLanguage (\ s a -> s{_dtacLanguage = a});

instance AWSRequest DescribeTrustedAdvisorChecks
         where
        type Sv DescribeTrustedAdvisorChecks = Support
        type Rs DescribeTrustedAdvisorChecks =
             DescribeTrustedAdvisorChecksResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorChecksResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "checks" .!@ mempty))

instance ToHeaders DescribeTrustedAdvisorChecks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeTrustedAdvisorChecks"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrustedAdvisorChecks where
        toJSON DescribeTrustedAdvisorChecks'{..}
          = object ["language" .= _dtacLanguage]

instance ToPath DescribeTrustedAdvisorChecks where
        toPath = const mempty

instance ToQuery DescribeTrustedAdvisorChecks where
        toQuery = const mempty

-- | Information about the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorChecks operation.
--
-- /See:/ 'describeTrustedAdvisorChecksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrsStatus'
--
-- * 'dtacrsChecks'
data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse'
    { _dtacrsStatus :: !Int
    , _dtacrsChecks :: ![TrustedAdvisorCheckDescription]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorChecksResponse' smart constructor.
describeTrustedAdvisorChecksResponse :: Int -> DescribeTrustedAdvisorChecksResponse
describeTrustedAdvisorChecksResponse pStatus_ =
    DescribeTrustedAdvisorChecksResponse'
    { _dtacrsStatus = pStatus_
    , _dtacrsChecks = mempty
    }

-- | FIXME: Undocumented member.
dtacrsStatus :: Lens' DescribeTrustedAdvisorChecksResponse Int
dtacrsStatus = lens _dtacrsStatus (\ s a -> s{_dtacrsStatus = a});

-- | Information about all available Trusted Advisor checks.
dtacrsChecks :: Lens' DescribeTrustedAdvisorChecksResponse [TrustedAdvisorCheckDescription]
dtacrsChecks = lens _dtacrsChecks (\ s a -> s{_dtacrsChecks = a}) . _Coerce;
