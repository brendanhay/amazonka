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
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or @text-array@ field to define language-specific text processing options. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DefineAnalysisScheme
    (
    -- * Creating a Request
      defineAnalysisScheme
    , DefineAnalysisScheme
    -- * Request Lenses
    , dasaDomainName
    , dasaAnalysisScheme

    -- * Destructuring the Response
    , defineAnalysisSchemeResponse
    , DefineAnalysisSchemeResponse
    -- * Response Lenses
    , defersResponseStatus
    , defersAnalysisScheme
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DefineAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme configuration.
--
--
--
-- /See:/ 'defineAnalysisScheme' smart constructor.
data DefineAnalysisScheme = DefineAnalysisScheme'
  { _dasaDomainName     :: !Text
  , _dasaAnalysisScheme :: !AnalysisScheme
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineAnalysisScheme' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasaDomainName' - Undocumented member.
--
-- * 'dasaAnalysisScheme' - Undocumented member.
defineAnalysisScheme
    :: Text -- ^ 'dasaDomainName'
    -> AnalysisScheme -- ^ 'dasaAnalysisScheme'
    -> DefineAnalysisScheme
defineAnalysisScheme pDomainName_ pAnalysisScheme_ =
  DefineAnalysisScheme'
    {_dasaDomainName = pDomainName_, _dasaAnalysisScheme = pAnalysisScheme_}


-- | Undocumented member.
dasaDomainName :: Lens' DefineAnalysisScheme Text
dasaDomainName = lens _dasaDomainName (\ s a -> s{_dasaDomainName = a})

-- | Undocumented member.
dasaAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
dasaAnalysisScheme = lens _dasaAnalysisScheme (\ s a -> s{_dasaAnalysisScheme = a})

instance AWSRequest DefineAnalysisScheme where
        type Rs DefineAnalysisScheme =
             DefineAnalysisSchemeResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DefineAnalysisSchemeResult"
              (\ s h x ->
                 DefineAnalysisSchemeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AnalysisScheme"))

instance Hashable DefineAnalysisScheme where

instance NFData DefineAnalysisScheme where

instance ToHeaders DefineAnalysisScheme where
        toHeaders = const mempty

instance ToPath DefineAnalysisScheme where
        toPath = const "/"

instance ToQuery DefineAnalysisScheme where
        toQuery DefineAnalysisScheme'{..}
          = mconcat
              ["Action" =: ("DefineAnalysisScheme" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dasaDomainName,
               "AnalysisScheme" =: _dasaAnalysisScheme]

-- | The result of a @'DefineAnalysisScheme' @ request. Contains the status of the newly-configured analysis scheme.
--
--
--
-- /See:/ 'defineAnalysisSchemeResponse' smart constructor.
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
  { _defersResponseStatus :: !Int
  , _defersAnalysisScheme :: !AnalysisSchemeStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defersResponseStatus' - -- | The response status code.
--
-- * 'defersAnalysisScheme' - Undocumented member.
defineAnalysisSchemeResponse
    :: Int -- ^ 'defersResponseStatus'
    -> AnalysisSchemeStatus -- ^ 'defersAnalysisScheme'
    -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse pResponseStatus_ pAnalysisScheme_ =
  DefineAnalysisSchemeResponse'
    { _defersResponseStatus = pResponseStatus_
    , _defersAnalysisScheme = pAnalysisScheme_
    }


-- | -- | The response status code.
defersResponseStatus :: Lens' DefineAnalysisSchemeResponse Int
defersResponseStatus = lens _defersResponseStatus (\ s a -> s{_defersResponseStatus = a})

-- | Undocumented member.
defersAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
defersAnalysisScheme = lens _defersAnalysisScheme (\ s a -> s{_defersAnalysisScheme = a})

instance NFData DefineAnalysisSchemeResponse where
