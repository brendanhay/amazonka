{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DefineSuggester
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

-- | Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you
-- want to search for possible matches and a unique name for the suggester.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineSuggester.html>
module Network.AWS.CloudSearch.DefineSuggester
    (
    -- * Request
      DefineSuggester
    -- ** Request constructor
    , defineSuggester
    -- ** Request lenses
    , dDomainName
    , dSuggester

    -- * Response
    , DefineSuggesterResponse
    -- ** Response constructor
    , defineSuggesterResponse
    -- ** Response lenses
    , dsrStatus
    , dsrSuggester
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DefineSuggester@ operation.
-- Specifies the name of the domain you want to update and the suggester
-- configuration.
--
-- /See:/ 'defineSuggester' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDomainName'
--
-- * 'dSuggester'
data DefineSuggester = DefineSuggester'
    { _dDomainName :: !Text
    , _dSuggester  :: !Suggester
    } deriving (Eq,Read,Show)

-- | 'DefineSuggester' smart constructor.
defineSuggester :: Text -> Suggester -> DefineSuggester
defineSuggester pDomainName pSuggester =
    DefineSuggester'
    { _dDomainName = pDomainName
    , _dSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
dDomainName :: Lens' DefineSuggester Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a});

-- | FIXME: Undocumented member.
dSuggester :: Lens' DefineSuggester Suggester
dSuggester = lens _dSuggester (\ s a -> s{_dSuggester = a});

instance AWSRequest DefineSuggester where
        type Sv DefineSuggester = CloudSearch
        type Rs DefineSuggester = DefineSuggesterResponse
        request = post
        response
          = receiveXMLWrapper "DefineSuggesterResult"
              (\ s h x ->
                 DefineSuggesterResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Suggester"))

instance ToHeaders DefineSuggester where
        toHeaders = const mempty

instance ToPath DefineSuggester where
        toPath = const "/"

instance ToQuery DefineSuggester where
        toQuery DefineSuggester'{..}
          = mconcat
              ["Action" =: ("DefineSuggester" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dDomainName,
               "Suggester" =: _dSuggester]

-- | The result of a @DefineSuggester@ request. Contains the status of the
-- newly-configured suggester.
--
-- /See:/ 'defineSuggesterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStatus'
--
-- * 'dsrSuggester'
data DefineSuggesterResponse = DefineSuggesterResponse'
    { _dsrStatus    :: !Int
    , _dsrSuggester :: !SuggesterStatus
    } deriving (Eq,Read,Show)

-- | 'DefineSuggesterResponse' smart constructor.
defineSuggesterResponse :: Int -> SuggesterStatus -> DefineSuggesterResponse
defineSuggesterResponse pStatus pSuggester =
    DefineSuggesterResponse'
    { _dsrStatus = pStatus
    , _dsrSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DefineSuggesterResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});

-- | FIXME: Undocumented member.
dsrSuggester :: Lens' DefineSuggesterResponse SuggesterStatus
dsrSuggester = lens _dsrSuggester (\ s a -> s{_dsrSuggester = a});
