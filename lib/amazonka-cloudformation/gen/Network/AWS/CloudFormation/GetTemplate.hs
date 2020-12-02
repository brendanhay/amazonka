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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template body for a specified stack. You can get the template for running or deleted stacks.
--
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days after the stack has been deleted.
--
module Network.AWS.CloudFormation.GetTemplate
    (
    -- * Creating a Request
      getTemplate
    , GetTemplate
    -- * Request Lenses
    , gtChangeSetName
    , gtTemplateStage
    , gtStackName

    -- * Destructuring the Response
    , getTemplateResponse
    , GetTemplateResponse
    -- * Response Lenses
    , gtrsStagesAvailable
    , gtrsTemplateBody
    , gtrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for a 'GetTemplate' action.
--
--
--
-- /See:/ 'getTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { _gtChangeSetName :: !(Maybe Text)
  , _gtTemplateStage :: !(Maybe TemplateStage)
  , _gtStackName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtChangeSetName' - The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
--
-- * 'gtTemplateStage' - For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .  If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
--
-- * 'gtStackName' - The name or the unique stack ID that is associated with the stack, which are not always interchangeable:     * Running stacks: You can specify either the stack's name or its unique stack ID.     * Deleted stacks: You must specify the unique stack ID. Default: There is no default value.
getTemplate
    :: GetTemplate
getTemplate =
  GetTemplate'
    { _gtChangeSetName = Nothing
    , _gtTemplateStage = Nothing
    , _gtStackName = Nothing
    }


-- | The name or Amazon Resource Name (ARN) of a change set for which AWS CloudFormation returns the associated template. If you specify a name, you must also specify the @StackName@ .
gtChangeSetName :: Lens' GetTemplate (Maybe Text)
gtChangeSetName = lens _gtChangeSetName (\ s a -> s{_gtChangeSetName = a})

-- | For templates that include transforms, the stage of the template that AWS CloudFormation returns. To get the user-submitted template, specify @Original@ . To get the template after AWS CloudFormation has processed all transforms, specify @Processed@ .  If the template doesn't include transforms, @Original@ and @Processed@ return the same template. By default, AWS CloudFormation specifies @Original@ .
gtTemplateStage :: Lens' GetTemplate (Maybe TemplateStage)
gtTemplateStage = lens _gtTemplateStage (\ s a -> s{_gtTemplateStage = a})

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:     * Running stacks: You can specify either the stack's name or its unique stack ID.     * Deleted stacks: You must specify the unique stack ID. Default: There is no default value.
gtStackName :: Lens' GetTemplate (Maybe Text)
gtStackName = lens _gtStackName (\ s a -> s{_gtStackName = a})

instance AWSRequest GetTemplate where
        type Rs GetTemplate = GetTemplateResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "GetTemplateResult"
              (\ s h x ->
                 GetTemplateResponse' <$>
                   (x .@? "StagesAvailable" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "TemplateBody")
                     <*> (pure (fromEnum s)))

instance Hashable GetTemplate where

instance NFData GetTemplate where

instance ToHeaders GetTemplate where
        toHeaders = const mempty

instance ToPath GetTemplate where
        toPath = const "/"

instance ToQuery GetTemplate where
        toQuery GetTemplate'{..}
          = mconcat
              ["Action" =: ("GetTemplate" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "ChangeSetName" =: _gtChangeSetName,
               "TemplateStage" =: _gtTemplateStage,
               "StackName" =: _gtStackName]

-- | The output for 'GetTemplate' action.
--
--
--
-- /See:/ 'getTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { _gtrsStagesAvailable :: !(Maybe [TemplateStage])
  , _gtrsTemplateBody    :: !(Maybe Text)
  , _gtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsStagesAvailable' - The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
--
-- * 'gtrsTemplateBody' - Structure containing the template body. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) AWS CloudFormation returns the same template that was used when the stack was created.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTemplateResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTemplateResponse
getTemplateResponse pResponseStatus_ =
  GetTemplateResponse'
    { _gtrsStagesAvailable = Nothing
    , _gtrsTemplateBody = Nothing
    , _gtrsResponseStatus = pResponseStatus_
    }


-- | The stage of the template that you can retrieve. For stacks, the @Original@ and @Processed@ templates are always available. For change sets, the @Original@ template is always available. After AWS CloudFormation finishes creating the change set, the @Processed@ template becomes available.
gtrsStagesAvailable :: Lens' GetTemplateResponse [TemplateStage]
gtrsStagesAvailable = lens _gtrsStagesAvailable (\ s a -> s{_gtrsStagesAvailable = a}) . _Default . _Coerce

-- | Structure containing the template body. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) AWS CloudFormation returns the same template that was used when the stack was created.
gtrsTemplateBody :: Lens' GetTemplateResponse (Maybe Text)
gtrsTemplateBody = lens _gtrsTemplateBody (\ s a -> s{_gtrsTemplateBody = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTemplateResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTemplateResponse where
