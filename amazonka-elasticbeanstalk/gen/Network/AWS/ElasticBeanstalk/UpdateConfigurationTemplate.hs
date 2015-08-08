{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration template to have the specified
-- properties or configuration option values.
--
-- If a property (for example, @ApplicationName@) is not provided, its
-- value remains unchanged. To clear such properties, specify an empty
-- string.
--
-- Related Topics
--
-- -   DescribeConfigurationOptions
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateConfigurationTemplate.html AWS API Reference> for UpdateConfigurationTemplate.
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Creating a Request
      UpdateConfigurationTemplate
    , updateConfigurationTemplate
    -- * Request Lenses
    , uctOptionsToRemove
    , uctOptionSettings
    , uctDescription
    , uctApplicationName
    , uctTemplateName

    -- * Destructuring the Response
    , ConfigurationSettingsDescription
    , configurationSettingsDescription
    -- * Response Lenses
    , csdTemplateName
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The result message containing the options for the specified solution
-- stack.
--
-- /See:/ 'updateConfigurationTemplate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uctOptionsToRemove'
--
-- * 'uctOptionSettings'
--
-- * 'uctDescription'
--
-- * 'uctApplicationName'
--
-- * 'uctTemplateName'
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
    { _uctOptionsToRemove :: !(Maybe [OptionSpecification])
    , _uctOptionSettings  :: !(Maybe [ConfigurationOptionSetting])
    , _uctDescription     :: !(Maybe Text)
    , _uctApplicationName :: !Text
    , _uctTemplateName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateConfigurationTemplate' smart constructor.
updateConfigurationTemplate :: Text -> Text -> UpdateConfigurationTemplate
updateConfigurationTemplate pApplicationName_ pTemplateName_ =
    UpdateConfigurationTemplate'
    { _uctOptionsToRemove = Nothing
    , _uctOptionSettings = Nothing
    , _uctDescription = Nothing
    , _uctApplicationName = pApplicationName_
    , _uctTemplateName = pTemplateName_
    }

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
uctOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctOptionsToRemove = lens _uctOptionsToRemove (\ s a -> s{_uctOptionsToRemove = a}) . _Default . _Coerce;

-- | A list of configuration option settings to update with the new specified
-- option value.
uctOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctOptionSettings = lens _uctOptionSettings (\ s a -> s{_uctOptionSettings = a}) . _Default . _Coerce;

-- | A new description for the configuration.
uctDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctDescription = lens _uctDescription (\ s a -> s{_uctDescription = a});

-- | The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
uctApplicationName :: Lens' UpdateConfigurationTemplate Text
uctApplicationName = lens _uctApplicationName (\ s a -> s{_uctApplicationName = a});

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
uctTemplateName :: Lens' UpdateConfigurationTemplate Text
uctTemplateName = lens _uctTemplateName (\ s a -> s{_uctTemplateName = a});

instance AWSRequest UpdateConfigurationTemplate where
        type Sv UpdateConfigurationTemplate =
             ElasticBeanstalk
        type Rs UpdateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = postQuery
        response
          = receiveXMLWrapper
              "UpdateConfigurationTemplateResult"
              (\ s h x -> parseXML x)

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
