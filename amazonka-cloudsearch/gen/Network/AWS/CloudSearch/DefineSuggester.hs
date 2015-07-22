{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures a suggester for a domain. A suggester enables you to display
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
    , defrqDomainName
    , defrqSuggester

    -- * Response
    , DefineSuggesterResponse
    -- ** Response constructor
    , defineSuggesterResponse
    -- ** Response lenses
    , dsrsStatus
    , dsrsSuggester
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
-- * 'defrqDomainName'
--
-- * 'defrqSuggester'
data DefineSuggester = DefineSuggester'
    { _defrqDomainName :: !Text
    , _defrqSuggester  :: !Suggester
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineSuggester' smart constructor.
defineSuggester :: Text -> Suggester -> DefineSuggester
defineSuggester pDomainName pSuggester =
    DefineSuggester'
    { _defrqDomainName = pDomainName
    , _defrqSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
defrqDomainName :: Lens' DefineSuggester Text
defrqDomainName = lens _defrqDomainName (\ s a -> s{_defrqDomainName = a});

-- | FIXME: Undocumented member.
defrqSuggester :: Lens' DefineSuggester Suggester
defrqSuggester = lens _defrqSuggester (\ s a -> s{_defrqSuggester = a});

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
               "DomainName" =: _defrqDomainName,
               "Suggester" =: _defrqSuggester]

-- | The result of a @DefineSuggester@ request. Contains the status of the
-- newly-configured suggester.
--
-- /See:/ 'defineSuggesterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsStatus'
--
-- * 'dsrsSuggester'
data DefineSuggesterResponse = DefineSuggesterResponse'
    { _dsrsStatus    :: !Int
    , _dsrsSuggester :: !SuggesterStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineSuggesterResponse' smart constructor.
defineSuggesterResponse :: Int -> SuggesterStatus -> DefineSuggesterResponse
defineSuggesterResponse pStatus pSuggester =
    DefineSuggesterResponse'
    { _dsrsStatus = pStatus
    , _dsrsSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
dsrsStatus :: Lens' DefineSuggesterResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});

-- | FIXME: Undocumented member.
dsrsSuggester :: Lens' DefineSuggesterResponse SuggesterStatus
dsrsSuggester = lens _dsrsSuggester (\ s a -> s{_dsrsSuggester = a});
