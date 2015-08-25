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
-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indexes the search suggestions. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_BuildSuggesters.html AWS API Reference> for BuildSuggesters.
module Network.AWS.CloudSearch.BuildSuggesters
    (
    -- * Creating a Request
      buildSuggesters
    , BuildSuggesters
    -- * Request Lenses
    , bsDomainName

    -- * Destructuring the Response
    , buildSuggestersResponse
    , BuildSuggestersResponse
    -- * Response Lenses
    , bsrsFieldNames
    , bsrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'BuildSuggester' operation.
-- Specifies the name of the domain you want to update.
--
-- /See:/ 'buildSuggesters' smart constructor.
newtype BuildSuggesters = BuildSuggesters'
    { _bsDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildSuggesters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsDomainName'
buildSuggesters
    :: Text -- ^ 'bsDomainName'
    -> BuildSuggesters
buildSuggesters pDomainName_ =
    BuildSuggesters'
    { _bsDomainName = pDomainName_
    }

-- | Undocumented member.
bsDomainName :: Lens' BuildSuggesters Text
bsDomainName = lens _bsDomainName (\ s a -> s{_bsDomainName = a});

instance AWSRequest BuildSuggesters where
        type Rs BuildSuggesters = BuildSuggestersResponse
        request = postQuery cloudSearch
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
               "DomainName" =: _bsDomainName]

-- | The result of a 'BuildSuggester' request. Contains a list of the fields
-- used for suggestions.
--
-- /See:/ 'buildSuggestersResponse' smart constructor.
data BuildSuggestersResponse = BuildSuggestersResponse'
    { _bsrsFieldNames :: !(Maybe [Text])
    , _bsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildSuggestersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsrsFieldNames'
--
-- * 'bsrsStatus'
buildSuggestersResponse
    :: Int -- ^ 'bsrsStatus'
    -> BuildSuggestersResponse
buildSuggestersResponse pStatus_ =
    BuildSuggestersResponse'
    { _bsrsFieldNames = Nothing
    , _bsrsStatus = pStatus_
    }

-- | Undocumented member.
bsrsFieldNames :: Lens' BuildSuggestersResponse [Text]
bsrsFieldNames = lens _bsrsFieldNames (\ s a -> s{_bsrsFieldNames = a}) . _Default . _Coerce;

-- | The response status code.
bsrsStatus :: Lens' BuildSuggestersResponse Int
bsrsStatus = lens _bsrsStatus (\ s a -> s{_bsrsStatus = a});
