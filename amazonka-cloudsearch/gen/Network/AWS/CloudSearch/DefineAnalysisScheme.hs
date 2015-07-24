{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or
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
    , dasaDomainName
    , dasaAnalysisScheme

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response constructor
    , defineAnalysisSchemeResponse
    -- ** Response lenses
    , defersStatus
    , defersAnalysisScheme
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DefineAnalysisScheme@ operation.
-- Specifies the name of the domain you want to update and the analysis
-- scheme configuration.
--
-- /See:/ 'defineAnalysisScheme' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasaDomainName'
--
-- * 'dasaAnalysisScheme'
data DefineAnalysisScheme = DefineAnalysisScheme'
    { _dasaDomainName     :: !Text
    , _dasaAnalysisScheme :: !AnalysisScheme
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineAnalysisScheme' smart constructor.
defineAnalysisScheme :: Text -> AnalysisScheme -> DefineAnalysisScheme
defineAnalysisScheme pDomainName_ pAnalysisScheme_ =
    DefineAnalysisScheme'
    { _dasaDomainName = pDomainName_
    , _dasaAnalysisScheme = pAnalysisScheme_
    }

-- | FIXME: Undocumented member.
dasaDomainName :: Lens' DefineAnalysisScheme Text
dasaDomainName = lens _dasaDomainName (\ s a -> s{_dasaDomainName = a});

-- | FIXME: Undocumented member.
dasaAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
dasaAnalysisScheme = lens _dasaAnalysisScheme (\ s a -> s{_dasaAnalysisScheme = a});

instance AWSRequest DefineAnalysisScheme where
        type Sv DefineAnalysisScheme = CloudSearch
        type Rs DefineAnalysisScheme =
             DefineAnalysisSchemeResponse
        request = post
        response
          = receiveXMLWrapper "DefineAnalysisSchemeResult"
              (\ s h x ->
                 DefineAnalysisSchemeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AnalysisScheme"))

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

-- | The result of a @DefineAnalysisScheme@ request. Contains the status of
-- the newly-configured analysis scheme.
--
-- /See:/ 'defineAnalysisSchemeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defersStatus'
--
-- * 'defersAnalysisScheme'
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
    { _defersStatus         :: !Int
    , _defersAnalysisScheme :: !AnalysisSchemeStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineAnalysisSchemeResponse' smart constructor.
defineAnalysisSchemeResponse :: Int -> AnalysisSchemeStatus -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse pStatus_ pAnalysisScheme_ =
    DefineAnalysisSchemeResponse'
    { _defersStatus = pStatus_
    , _defersAnalysisScheme = pAnalysisScheme_
    }

-- | FIXME: Undocumented member.
defersStatus :: Lens' DefineAnalysisSchemeResponse Int
defersStatus = lens _defersStatus (\ s a -> s{_defersStatus = a});

-- | FIXME: Undocumented member.
defersAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
defersAnalysisScheme = lens _defersAnalysisScheme (\ s a -> s{_defersAnalysisScheme = a});
