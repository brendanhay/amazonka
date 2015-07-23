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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateConfigurationTemplate.html>
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Request
      UpdateConfigurationTemplate
    -- ** Request constructor
    , updateConfigurationTemplate
    -- ** Request lenses
    , uctrqOptionsToRemove
    , uctrqOptionSettings
    , uctrqDescription
    , uctrqApplicationName
    , uctrqTemplateName

    -- * Response
    , ConfigurationSettingsDescription
    -- ** Response constructor
    , configurationSettingsDescription
    -- ** Response lenses
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
-- * 'uctrqOptionsToRemove'
--
-- * 'uctrqOptionSettings'
--
-- * 'uctrqDescription'
--
-- * 'uctrqApplicationName'
--
-- * 'uctrqTemplateName'
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
    { _uctrqOptionsToRemove :: !(Maybe [OptionSpecification])
    , _uctrqOptionSettings  :: !(Maybe [ConfigurationOptionSetting])
    , _uctrqDescription     :: !(Maybe Text)
    , _uctrqApplicationName :: !Text
    , _uctrqTemplateName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateConfigurationTemplate' smart constructor.
updateConfigurationTemplate :: Text -> Text -> UpdateConfigurationTemplate
updateConfigurationTemplate pApplicationName_ pTemplateName_ =
    UpdateConfigurationTemplate'
    { _uctrqOptionsToRemove = Nothing
    , _uctrqOptionSettings = Nothing
    , _uctrqDescription = Nothing
    , _uctrqApplicationName = pApplicationName_
    , _uctrqTemplateName = pTemplateName_
    }

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
uctrqOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctrqOptionsToRemove = lens _uctrqOptionsToRemove (\ s a -> s{_uctrqOptionsToRemove = a}) . _Default;

-- | A list of configuration option settings to update with the new specified
-- option value.
uctrqOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctrqOptionSettings = lens _uctrqOptionSettings (\ s a -> s{_uctrqOptionSettings = a}) . _Default;

-- | A new description for the configuration.
uctrqDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctrqDescription = lens _uctrqDescription (\ s a -> s{_uctrqDescription = a});

-- | The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
uctrqApplicationName :: Lens' UpdateConfigurationTemplate Text
uctrqApplicationName = lens _uctrqApplicationName (\ s a -> s{_uctrqApplicationName = a});

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
uctrqTemplateName :: Lens' UpdateConfigurationTemplate Text
uctrqTemplateName = lens _uctrqTemplateName (\ s a -> s{_uctrqTemplateName = a});

instance AWSRequest UpdateConfigurationTemplate where
        type Sv UpdateConfigurationTemplate =
             ElasticBeanstalk
        type Rs UpdateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = post
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
                   (toQueryList "member" <$> _uctrqOptionsToRemove),
               "OptionSettings" =:
                 toQuery
                   (toQueryList "member" <$> _uctrqOptionSettings),
               "Description" =: _uctrqDescription,
               "ApplicationName" =: _uctrqApplicationName,
               "TemplateName" =: _uctrqTemplateName]
