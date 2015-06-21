{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
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

-- | Configures an analysis scheme that can be applied to a @text@ or
-- @text-array@ field to define language-specific text processing options.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineAnalysisScheme.html>
module Network.AWS.CloudSearch.DefineAnalysisScheme
    (
    -- * Request
      DefineAnalysisScheme
    -- ** Request constructor
    , defineAnalysisScheme
    -- ** Request lenses
    , defiDomainName
    , defiAnalysisScheme

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response constructor
    , defineAnalysisSchemeResponse
    -- ** Response lenses
    , defAnalysisScheme
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'defineAnalysisScheme' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defiDomainName'
--
-- * 'defiAnalysisScheme'
data DefineAnalysisScheme = DefineAnalysisScheme'{_defiDomainName :: Text, _defiAnalysisScheme :: AnalysisScheme} deriving (Eq, Read, Show)

-- | 'DefineAnalysisScheme' smart constructor.
defineAnalysisScheme :: Text -> AnalysisScheme -> DefineAnalysisScheme
defineAnalysisScheme pDomainName pAnalysisScheme = DefineAnalysisScheme'{_defiDomainName = pDomainName, _defiAnalysisScheme = pAnalysisScheme};

-- | FIXME: Undocumented member.
defiDomainName :: Lens' DefineAnalysisScheme Text
defiDomainName = lens _defiDomainName (\ s a -> s{_defiDomainName = a});

-- | FIXME: Undocumented member.
defiAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
defiAnalysisScheme = lens _defiAnalysisScheme (\ s a -> s{_defiAnalysisScheme = a});

instance AWSRequest DefineAnalysisScheme where
        type Sv DefineAnalysisScheme = CloudSearch
        type Rs DefineAnalysisScheme =
             DefineAnalysisSchemeResponse
        request = post
        response
          = receiveXMLWrapper "DefineAnalysisSchemeResult"
              (\ s h x ->
                 DefineAnalysisSchemeResponse' <$>
                   (x .@ "AnalysisScheme"))

instance ToHeaders DefineAnalysisScheme where
        toHeaders = const mempty

instance ToPath DefineAnalysisScheme where
        toPath = const "/"

instance ToQuery DefineAnalysisScheme where
        toQuery DefineAnalysisScheme'{..}
          = mconcat
              ["Action" =: ("DefineAnalysisScheme" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _defiDomainName,
               "AnalysisScheme" =: _defiAnalysisScheme]

-- | /See:/ 'defineAnalysisSchemeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defAnalysisScheme'
newtype DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'{_defAnalysisScheme :: AnalysisSchemeStatus} deriving (Eq, Read, Show)

-- | 'DefineAnalysisSchemeResponse' smart constructor.
defineAnalysisSchemeResponse :: AnalysisSchemeStatus -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse pAnalysisScheme = DefineAnalysisSchemeResponse'{_defAnalysisScheme = pAnalysisScheme};

-- | FIXME: Undocumented member.
defAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
defAnalysisScheme = lens _defAnalysisScheme (\ s a -> s{_defAnalysisScheme = a});
