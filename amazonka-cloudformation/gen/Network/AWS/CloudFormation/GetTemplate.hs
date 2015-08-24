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
-- Module      : Network.AWS.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template body for a specified stack. You can get the
-- template for running or deleted stacks.
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days
-- after the stack has been deleted.
--
-- If the template does not exist, a 'ValidationError' is returned.
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplate.html AWS API Reference> for GetTemplate.
module Network.AWS.CloudFormation.GetTemplate
    (
    -- * Creating a Request
      getTemplate
    , GetTemplate
    -- * Request Lenses
    , gtStackName

    -- * Destructuring the Response
    , getTemplateResponse
    , GetTemplateResponse
    -- * Response Lenses
    , gtrsTemplateBody
    , gtrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for a GetTemplate action.
--
-- /See:/ 'getTemplate' smart constructor.
newtype GetTemplate = GetTemplate'
    { _gtStackName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtStackName'
getTemplate
    :: Text -- ^ 'gtStackName'
    -> GetTemplate
getTemplate pStackName_ =
    GetTemplate'
    { _gtStackName = pStackName_
    }

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
        type Rs GetTemplate = GetTemplateResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "GetTemplateResult"
              (\ s h x ->
                 GetTemplateResponse' <$>
                   (x .@? "TemplateBody") <*> (pure (fromEnum s)))

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

-- | The output for GetTemplate action.
--
-- /See:/ 'getTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
    { _gtrsTemplateBody :: !(Maybe Text)
    , _gtrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTemplateBody'
--
-- * 'gtrsStatus'
getTemplateResponse
    :: Int -- ^ 'gtrsStatus'
    -> GetTemplateResponse
getTemplateResponse pStatus_ =
    GetTemplateResponse'
    { _gtrsTemplateBody = Nothing
    , _gtrsStatus = pStatus_
    }

-- | Structure containing the template body. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
gtrsTemplateBody :: Lens' GetTemplateResponse (Maybe Text)
gtrsTemplateBody = lens _gtrsTemplateBody (\ s a -> s{_gtrsTemplateBody = a});

-- | The response status code.
gtrsStatus :: Lens' GetTemplateResponse Int
gtrsStatus = lens _gtrsStatus (\ s a -> s{_gtrsStatus = a});
