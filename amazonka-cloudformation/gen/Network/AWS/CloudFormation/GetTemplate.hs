{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.GetTemplate
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

-- | Returns the template body for a specified stack. You can get the
-- template for running or deleted stacks.
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days
-- after the stack has been deleted.
--
-- If the template does not exist, a @ValidationError@ is returned.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplate.html>
module Network.AWS.CloudFormation.GetTemplate
    (
    -- * Request
      GetTemplate
    -- ** Request constructor
    , getTemplate
    -- ** Request lenses
    , gtStackName

    -- * Response
    , GetTemplateResponse
    -- ** Response constructor
    , getTemplateResponse
    -- ** Response lenses
    , gtrTemplateBody
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFormation.Types

-- | /See:/ 'getTemplate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtStackName'
newtype GetTemplate = GetTemplate'{_gtStackName :: Text} deriving (Eq, Read, Show)

-- | 'GetTemplate' smart constructor.
getTemplate :: Text -> GetTemplate
getTemplate pStackName = GetTemplate'{_gtStackName = pStackName};

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
gtStackName :: Lens' GetTemplate Text
gtStackName = lens _gtStackName (\ s a -> s{_gtStackName = a});

instance AWSRequest GetTemplate where
        type Sv GetTemplate = CloudFormation
        type Rs GetTemplate = GetTemplateResponse
        request = post
        response
          = receiveXMLWrapper "GetTemplateResult"
              (\ s h x ->
                 GetTemplateResponse' <$> x .@ "TemplateBody")

instance ToHeaders GetTemplate where
        toHeaders = const mempty

instance ToPath GetTemplate where
        toPath = const "/"

instance ToQuery GetTemplate where
        toQuery GetTemplate'{..}
          = mconcat
              ["Action" =: ("GetTemplate" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _gtStackName]

-- | /See:/ 'getTemplateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtrTemplateBody'
newtype GetTemplateResponse = GetTemplateResponse'{_gtrTemplateBody :: Text} deriving (Eq, Read, Show)

-- | 'GetTemplateResponse' smart constructor.
getTemplateResponse :: Text -> GetTemplateResponse
getTemplateResponse pTemplateBody = GetTemplateResponse'{_gtrTemplateBody = pTemplateBody};

-- | Structure containing the template body. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
gtrTemplateBody :: Lens' GetTemplateResponse Text
gtrTemplateBody = lens _gtrTemplateBody (\ s a -> s{_gtrTemplateBody = a});
