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
    , ddrqDomainName
    , ddrqAnalysisScheme

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response constructor
    , defineAnalysisSchemeResponse
    -- ** Response lenses
    , deffrsStatus
    , deffrsAnalysisScheme
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
-- * 'ddrqDomainName'
--
-- * 'ddrqAnalysisScheme'
data DefineAnalysisScheme = DefineAnalysisScheme'
    { _ddrqDomainName     :: !Text
    , _ddrqAnalysisScheme :: !AnalysisScheme
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineAnalysisScheme' smart constructor.
defineAnalysisScheme :: Text -> AnalysisScheme -> DefineAnalysisScheme
defineAnalysisScheme pDomainName_ pAnalysisScheme_ =
    DefineAnalysisScheme'
    { _ddrqDomainName = pDomainName_
    , _ddrqAnalysisScheme = pAnalysisScheme_
    }

-- | FIXME: Undocumented member.
ddrqDomainName :: Lens' DefineAnalysisScheme Text
ddrqDomainName = lens _ddrqDomainName (\ s a -> s{_ddrqDomainName = a});

-- | FIXME: Undocumented member.
ddrqAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
ddrqAnalysisScheme = lens _ddrqAnalysisScheme (\ s a -> s{_ddrqAnalysisScheme = a});

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
               "DomainName" =: _ddrqDomainName,
               "AnalysisScheme" =: _ddrqAnalysisScheme]

-- | The result of a @DefineAnalysisScheme@ request. Contains the status of
-- the newly-configured analysis scheme.
--
-- /See:/ 'defineAnalysisSchemeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deffrsStatus'
--
-- * 'deffrsAnalysisScheme'
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
    { _deffrsStatus         :: !Int
    , _deffrsAnalysisScheme :: !AnalysisSchemeStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineAnalysisSchemeResponse' smart constructor.
defineAnalysisSchemeResponse :: Int -> AnalysisSchemeStatus -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse pStatus_ pAnalysisScheme_ =
    DefineAnalysisSchemeResponse'
    { _deffrsStatus = pStatus_
    , _deffrsAnalysisScheme = pAnalysisScheme_
    }

-- | FIXME: Undocumented member.
deffrsStatus :: Lens' DefineAnalysisSchemeResponse Int
deffrsStatus = lens _deffrsStatus (\ s a -> s{_deffrsStatus = a});

-- | FIXME: Undocumented member.
deffrsAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
deffrsAnalysisScheme = lens _deffrsAnalysisScheme (\ s a -> s{_deffrsAnalysisScheme = a});
