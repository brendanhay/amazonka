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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration template to have the specified properties or configuration option values.
--
--
-- Related Topics
--
--     * 'DescribeConfigurationOptions'
--
--
--
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Creating a Request
      updateConfigurationTemplate
    , UpdateConfigurationTemplate
    -- * Request Lenses
    , uctOptionsToRemove
    , uctOptionSettings
    , uctDescription
    , uctApplicationName
    , uctTemplateName

    -- * Destructuring the Response
    , configurationSettingsDescription
    , ConfigurationSettingsDescription
    -- * Response Lenses
    , csdTemplateName
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdPlatformARN
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The result message containing the options for the specified solution stack.
--
--
--
-- /See:/ 'updateConfigurationTemplate' smart constructor.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
  { _uctOptionsToRemove :: !(Maybe [OptionSpecification])
  , _uctOptionSettings  :: !(Maybe [ConfigurationOptionSetting])
  , _uctDescription     :: !(Maybe Text)
  , _uctApplicationName :: !Text
  , _uctTemplateName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uctOptionsToRemove' - A list of configuration options to remove from the configuration set. Constraint: You can remove only @UserDefined@ configuration options.
--
-- * 'uctOptionSettings' - A list of configuration option settings to update with the new specified option value.
--
-- * 'uctDescription' - A new description for the configuration.
--
-- * 'uctApplicationName' - The name of the application associated with the configuration template to update. If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
--
-- * 'uctTemplateName' - The name of the configuration template to update. If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
updateConfigurationTemplate
    :: Text -- ^ 'uctApplicationName'
    -> Text -- ^ 'uctTemplateName'
    -> UpdateConfigurationTemplate
updateConfigurationTemplate pApplicationName_ pTemplateName_ =
  UpdateConfigurationTemplate'
    { _uctOptionsToRemove = Nothing
    , _uctOptionSettings = Nothing
    , _uctDescription = Nothing
    , _uctApplicationName = pApplicationName_
    , _uctTemplateName = pTemplateName_
    }


-- | A list of configuration options to remove from the configuration set. Constraint: You can remove only @UserDefined@ configuration options.
uctOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctOptionsToRemove = lens _uctOptionsToRemove (\ s a -> s{_uctOptionsToRemove = a}) . _Default . _Coerce

-- | A list of configuration option settings to update with the new specified option value.
uctOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctOptionSettings = lens _uctOptionSettings (\ s a -> s{_uctOptionSettings = a}) . _Default . _Coerce

-- | A new description for the configuration.
uctDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctDescription = lens _uctDescription (\ s a -> s{_uctDescription = a})

-- | The name of the application associated with the configuration template to update. If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
uctApplicationName :: Lens' UpdateConfigurationTemplate Text
uctApplicationName = lens _uctApplicationName (\ s a -> s{_uctApplicationName = a})

-- | The name of the configuration template to update. If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
uctTemplateName :: Lens' UpdateConfigurationTemplate Text
uctTemplateName = lens _uctTemplateName (\ s a -> s{_uctTemplateName = a})

instance AWSRequest UpdateConfigurationTemplate where
        type Rs UpdateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "UpdateConfigurationTemplateResult"
              (\ s h x -> parseXML x)

instance Hashable UpdateConfigurationTemplate where

instance NFData UpdateConfigurationTemplate where

instance ToHeaders UpdateConfigurationTemplate where
        toHeaders = const mempty

instance ToPath UpdateConfigurationTemplate where
        toPath = const "/"

instance ToQuery UpdateConfigurationTemplate where
        toQuery UpdateConfigurationTemplate'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _uctOptionsToRemove),
               "OptionSettings" =:
                 toQuery
                   (toQueryList "member" <$> _uctOptionSettings),
               "Description" =: _uctDescription,
               "ApplicationName" =: _uctApplicationName,
               "TemplateName" =: _uctTemplateName]
