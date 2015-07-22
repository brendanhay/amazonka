{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Indexes the search suggestions. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_BuildSuggesters.html>
module Network.AWS.CloudSearch.BuildSuggesters
    (
    -- * Request
      BuildSuggesters
    -- ** Request constructor
    , buildSuggesters
    -- ** Request lenses
    , bsrqDomainName

    -- * Response
    , BuildSuggestersResponse
    -- ** Response constructor
    , buildSuggestersResponse
    -- ** Response lenses
    , bsrsFieldNames
    , bsrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @BuildSuggester@ operation.
-- Specifies the name of the domain you want to update.
--
-- /See:/ 'buildSuggesters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsrqDomainName'
newtype BuildSuggesters = BuildSuggesters'
    { _bsrqDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BuildSuggesters' smart constructor.
buildSuggesters :: Text -> BuildSuggesters
buildSuggesters pDomainName =
    BuildSuggesters'
    { _bsrqDomainName = pDomainName
    }

-- | FIXME: Undocumented member.
bsrqDomainName :: Lens' BuildSuggesters Text
bsrqDomainName = lens _bsrqDomainName (\ s a -> s{_bsrqDomainName = a});

instance AWSRequest BuildSuggesters where
        type Sv BuildSuggesters = CloudSearch
        type Rs BuildSuggesters = BuildSuggestersResponse
        request = post
        response
          = receiveXMLWrapper "BuildSuggestersResult"
              (\ s h x ->
                 BuildSuggestersResponse' <$>
                   (x .@? "FieldNames" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders BuildSuggesters where
        toHeaders = const mempty

instance ToPath BuildSuggesters where
        toPath = const "/"

instance ToQuery BuildSuggesters where
        toQuery BuildSuggesters'{..}
          = mconcat
              ["Action" =: ("BuildSuggesters" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _bsrqDomainName]

-- | The result of a @BuildSuggester@ request. Contains a list of the fields
-- used for suggestions.
--
-- /See:/ 'buildSuggestersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsrsFieldNames'
--
-- * 'bsrsStatus'
data BuildSuggestersResponse = BuildSuggestersResponse'
    { _bsrsFieldNames :: !(Maybe [Text])
    , _bsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BuildSuggestersResponse' smart constructor.
buildSuggestersResponse :: Int -> BuildSuggestersResponse
buildSuggestersResponse pStatus =
    BuildSuggestersResponse'
    { _bsrsFieldNames = Nothing
    , _bsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
bsrsFieldNames :: Lens' BuildSuggestersResponse [Text]
bsrsFieldNames = lens _bsrsFieldNames (\ s a -> s{_bsrsFieldNames = a}) . _Default;

-- | FIXME: Undocumented member.
bsrsStatus :: Lens' BuildSuggestersResponse Int
bsrsStatus = lens _bsrsStatus (\ s a -> s{_bsrsStatus = a});
