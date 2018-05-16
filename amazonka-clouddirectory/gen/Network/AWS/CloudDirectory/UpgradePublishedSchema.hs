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
-- Module      : Network.AWS.CloudDirectory.UpgradePublishedSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a published schema under a new minor version revision using the current contents of @DevelopmentSchemaArn@ .
--
--
module Network.AWS.CloudDirectory.UpgradePublishedSchema
    (
    -- * Creating a Request
      upgradePublishedSchema
    , UpgradePublishedSchema
    -- * Request Lenses
    , upsDryRun
    , upsDevelopmentSchemaARN
    , upsPublishedSchemaARN
    , upsMinorVersion

    -- * Destructuring the Response
    , upgradePublishedSchemaResponse
    , UpgradePublishedSchemaResponse
    -- * Response Lenses
    , upsrsUpgradedSchemaARN
    , upsrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'upgradePublishedSchema' smart constructor.
data UpgradePublishedSchema = UpgradePublishedSchema'
  { _upsDryRun               :: !(Maybe Bool)
  , _upsDevelopmentSchemaARN :: !Text
  , _upsPublishedSchemaARN   :: !Text
  , _upsMinorVersion         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradePublishedSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsDryRun' - Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
--
-- * 'upsDevelopmentSchemaARN' - The ARN of the development schema with the changes used for the upgrade.
--
-- * 'upsPublishedSchemaARN' - The ARN of the published schema to be upgraded.
--
-- * 'upsMinorVersion' - Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
upgradePublishedSchema
    :: Text -- ^ 'upsDevelopmentSchemaARN'
    -> Text -- ^ 'upsPublishedSchemaARN'
    -> Text -- ^ 'upsMinorVersion'
    -> UpgradePublishedSchema
upgradePublishedSchema pDevelopmentSchemaARN_ pPublishedSchemaARN_ pMinorVersion_ =
  UpgradePublishedSchema'
    { _upsDryRun = Nothing
    , _upsDevelopmentSchemaARN = pDevelopmentSchemaARN_
    , _upsPublishedSchemaARN = pPublishedSchemaARN_
    , _upsMinorVersion = pMinorVersion_
    }


-- | Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
upsDryRun :: Lens' UpgradePublishedSchema (Maybe Bool)
upsDryRun = lens _upsDryRun (\ s a -> s{_upsDryRun = a})

-- | The ARN of the development schema with the changes used for the upgrade.
upsDevelopmentSchemaARN :: Lens' UpgradePublishedSchema Text
upsDevelopmentSchemaARN = lens _upsDevelopmentSchemaARN (\ s a -> s{_upsDevelopmentSchemaARN = a})

-- | The ARN of the published schema to be upgraded.
upsPublishedSchemaARN :: Lens' UpgradePublishedSchema Text
upsPublishedSchemaARN = lens _upsPublishedSchemaARN (\ s a -> s{_upsPublishedSchemaARN = a})

-- | Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
upsMinorVersion :: Lens' UpgradePublishedSchema Text
upsMinorVersion = lens _upsMinorVersion (\ s a -> s{_upsMinorVersion = a})

instance AWSRequest UpgradePublishedSchema where
        type Rs UpgradePublishedSchema =
             UpgradePublishedSchemaResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 UpgradePublishedSchemaResponse' <$>
                   (x .?> "UpgradedSchemaArn") <*> (pure (fromEnum s)))

instance Hashable UpgradePublishedSchema where

instance NFData UpgradePublishedSchema where

instance ToHeaders UpgradePublishedSchema where
        toHeaders = const mempty

instance ToJSON UpgradePublishedSchema where
        toJSON UpgradePublishedSchema'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _upsDryRun,
                  Just
                    ("DevelopmentSchemaArn" .= _upsDevelopmentSchemaARN),
                  Just
                    ("PublishedSchemaArn" .= _upsPublishedSchemaARN),
                  Just ("MinorVersion" .= _upsMinorVersion)])

instance ToPath UpgradePublishedSchema where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/upgradepublished"

instance ToQuery UpgradePublishedSchema where
        toQuery = const mempty

-- | /See:/ 'upgradePublishedSchemaResponse' smart constructor.
data UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse'
  { _upsrsUpgradedSchemaARN :: !(Maybe Text)
  , _upsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradePublishedSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsrsUpgradedSchemaARN' - The ARN of the upgraded schema that is returned as part of the response.
--
-- * 'upsrsResponseStatus' - -- | The response status code.
upgradePublishedSchemaResponse
    :: Int -- ^ 'upsrsResponseStatus'
    -> UpgradePublishedSchemaResponse
upgradePublishedSchemaResponse pResponseStatus_ =
  UpgradePublishedSchemaResponse'
    {_upsrsUpgradedSchemaARN = Nothing, _upsrsResponseStatus = pResponseStatus_}


-- | The ARN of the upgraded schema that is returned as part of the response.
upsrsUpgradedSchemaARN :: Lens' UpgradePublishedSchemaResponse (Maybe Text)
upsrsUpgradedSchemaARN = lens _upsrsUpgradedSchemaARN (\ s a -> s{_upsrsUpgradedSchemaARN = a})

-- | -- | The response status code.
upsrsResponseStatus :: Lens' UpgradePublishedSchemaResponse Int
upsrsResponseStatus = lens _upsrsResponseStatus (\ s a -> s{_upsrsResponseStatus = a})

instance NFData UpgradePublishedSchemaResponse where
