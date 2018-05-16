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
-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a suggester for a domain. A suggester enables you to display possible matches before users finish typing their queries. When you configure a suggester, you must specify the name of the text field you want to search for possible matches and a unique name for the suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DefineSuggester
    (
    -- * Creating a Request
      defineSuggester
    , DefineSuggester
    -- * Request Lenses
    , defDomainName
    , defSuggester

    -- * Destructuring the Response
    , defineSuggesterResponse
    , DefineSuggesterResponse
    -- * Response Lenses
    , dsrsResponseStatus
    , dsrsSuggester
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DefineSuggester' @ operation. Specifies the name of the domain you want to update and the suggester configuration.
--
--
--
-- /See:/ 'defineSuggester' smart constructor.
data DefineSuggester = DefineSuggester'
  { _defDomainName :: !Text
  , _defSuggester  :: !Suggester
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineSuggester' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defDomainName' - Undocumented member.
--
-- * 'defSuggester' - Undocumented member.
defineSuggester
    :: Text -- ^ 'defDomainName'
    -> Suggester -- ^ 'defSuggester'
    -> DefineSuggester
defineSuggester pDomainName_ pSuggester_ =
  DefineSuggester' {_defDomainName = pDomainName_, _defSuggester = pSuggester_}


-- | Undocumented member.
defDomainName :: Lens' DefineSuggester Text
defDomainName = lens _defDomainName (\ s a -> s{_defDomainName = a})

-- | Undocumented member.
defSuggester :: Lens' DefineSuggester Suggester
defSuggester = lens _defSuggester (\ s a -> s{_defSuggester = a})

instance AWSRequest DefineSuggester where
        type Rs DefineSuggester = DefineSuggesterResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DefineSuggesterResult"
              (\ s h x ->
                 DefineSuggesterResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Suggester"))

instance Hashable DefineSuggester where

instance NFData DefineSuggester where

instance ToHeaders DefineSuggester where
        toHeaders = const mempty

instance ToPath DefineSuggester where
        toPath = const "/"

instance ToQuery DefineSuggester where
        toQuery DefineSuggester'{..}
          = mconcat
              ["Action" =: ("DefineSuggester" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _defDomainName,
               "Suggester" =: _defSuggester]

-- | The result of a @DefineSuggester@ request. Contains the status of the newly-configured suggester.
--
--
--
-- /See:/ 'defineSuggesterResponse' smart constructor.
data DefineSuggesterResponse = DefineSuggesterResponse'
  { _dsrsResponseStatus :: !Int
  , _dsrsSuggester      :: !SuggesterStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineSuggesterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
--
-- * 'dsrsSuggester' - Undocumented member.
defineSuggesterResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> SuggesterStatus -- ^ 'dsrsSuggester'
    -> DefineSuggesterResponse
defineSuggesterResponse pResponseStatus_ pSuggester_ =
  DefineSuggesterResponse'
    {_dsrsResponseStatus = pResponseStatus_, _dsrsSuggester = pSuggester_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DefineSuggesterResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

-- | Undocumented member.
dsrsSuggester :: Lens' DefineSuggesterResponse SuggesterStatus
dsrsSuggester = lens _dsrsSuggester (\ s a -> s{_dsrsSuggester = a})

instance NFData DefineSuggesterResponse where
