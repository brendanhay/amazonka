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
-- Module      : Network.AWS.Config.PutConfigurationAggregator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the configuration aggregator with the selected source accounts and regions. The source account can be individual account(s) or an organization.
--
--
module Network.AWS.Config.PutConfigurationAggregator
    (
    -- * Creating a Request
      putConfigurationAggregator
    , PutConfigurationAggregator
    -- * Request Lenses
    , pcaOrganizationAggregationSource
    , pcaAccountAggregationSources
    , pcaConfigurationAggregatorName

    -- * Destructuring the Response
    , putConfigurationAggregatorResponse
    , PutConfigurationAggregatorResponse
    -- * Response Lenses
    , pcarsConfigurationAggregator
    , pcarsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putConfigurationAggregator' smart constructor.
data PutConfigurationAggregator = PutConfigurationAggregator'
  { _pcaOrganizationAggregationSource :: !(Maybe OrganizationAggregationSource)
  , _pcaAccountAggregationSources     :: !(Maybe [AccountAggregationSource])
  , _pcaConfigurationAggregatorName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConfigurationAggregator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcaOrganizationAggregationSource' - An OrganizationAggregationSource object.
--
-- * 'pcaAccountAggregationSources' - A list of AccountAggregationSource object.
--
-- * 'pcaConfigurationAggregatorName' - The name of the configuration aggregator.
putConfigurationAggregator
    :: Text -- ^ 'pcaConfigurationAggregatorName'
    -> PutConfigurationAggregator
putConfigurationAggregator pConfigurationAggregatorName_ =
  PutConfigurationAggregator'
    { _pcaOrganizationAggregationSource = Nothing
    , _pcaAccountAggregationSources = Nothing
    , _pcaConfigurationAggregatorName = pConfigurationAggregatorName_
    }


-- | An OrganizationAggregationSource object.
pcaOrganizationAggregationSource :: Lens' PutConfigurationAggregator (Maybe OrganizationAggregationSource)
pcaOrganizationAggregationSource = lens _pcaOrganizationAggregationSource (\ s a -> s{_pcaOrganizationAggregationSource = a})

-- | A list of AccountAggregationSource object.
pcaAccountAggregationSources :: Lens' PutConfigurationAggregator [AccountAggregationSource]
pcaAccountAggregationSources = lens _pcaAccountAggregationSources (\ s a -> s{_pcaAccountAggregationSources = a}) . _Default . _Coerce

-- | The name of the configuration aggregator.
pcaConfigurationAggregatorName :: Lens' PutConfigurationAggregator Text
pcaConfigurationAggregatorName = lens _pcaConfigurationAggregatorName (\ s a -> s{_pcaConfigurationAggregatorName = a})

instance AWSRequest PutConfigurationAggregator where
        type Rs PutConfigurationAggregator =
             PutConfigurationAggregatorResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 PutConfigurationAggregatorResponse' <$>
                   (x .?> "ConfigurationAggregator") <*>
                     (pure (fromEnum s)))

instance Hashable PutConfigurationAggregator where

instance NFData PutConfigurationAggregator where

instance ToHeaders PutConfigurationAggregator where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutConfigurationAggregator" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutConfigurationAggregator where
        toJSON PutConfigurationAggregator'{..}
          = object
              (catMaybes
                 [("OrganizationAggregationSource" .=) <$>
                    _pcaOrganizationAggregationSource,
                  ("AccountAggregationSources" .=) <$>
                    _pcaAccountAggregationSources,
                  Just
                    ("ConfigurationAggregatorName" .=
                       _pcaConfigurationAggregatorName)])

instance ToPath PutConfigurationAggregator where
        toPath = const "/"

instance ToQuery PutConfigurationAggregator where
        toQuery = const mempty

-- | /See:/ 'putConfigurationAggregatorResponse' smart constructor.
data PutConfigurationAggregatorResponse = PutConfigurationAggregatorResponse'
  { _pcarsConfigurationAggregator :: !(Maybe ConfigurationAggregator)
  , _pcarsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConfigurationAggregatorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcarsConfigurationAggregator' - Returns a ConfigurationAggregator object.
--
-- * 'pcarsResponseStatus' - -- | The response status code.
putConfigurationAggregatorResponse
    :: Int -- ^ 'pcarsResponseStatus'
    -> PutConfigurationAggregatorResponse
putConfigurationAggregatorResponse pResponseStatus_ =
  PutConfigurationAggregatorResponse'
    { _pcarsConfigurationAggregator = Nothing
    , _pcarsResponseStatus = pResponseStatus_
    }


-- | Returns a ConfigurationAggregator object.
pcarsConfigurationAggregator :: Lens' PutConfigurationAggregatorResponse (Maybe ConfigurationAggregator)
pcarsConfigurationAggregator = lens _pcarsConfigurationAggregator (\ s a -> s{_pcarsConfigurationAggregator = a})

-- | -- | The response status code.
pcarsResponseStatus :: Lens' PutConfigurationAggregatorResponse Int
pcarsResponseStatus = lens _pcarsResponseStatus (\ s a -> s{_pcarsResponseStatus = a})

instance NFData PutConfigurationAggregatorResponse
         where
