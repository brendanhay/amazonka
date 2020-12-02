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
-- Module      : Network.AWS.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
--
--
module Network.AWS.CodeStar.DescribeProject
    (
    -- * Creating a Request
      describeProject
    , DescribeProject
    -- * Request Lenses
    , dId

    -- * Destructuring the Response
    , describeProjectResponse
    , DescribeProjectResponse
    -- * Response Lenses
    , drsArn
    , drsProjectTemplateId
    , drsName
    , drsId
    , drsStackId
    , drsClientRequestToken
    , drsCreatedTimeStamp
    , drsDescription
    , drsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeProject' smart constructor.
newtype DescribeProject = DescribeProject'
  { _dId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The ID of the project.
describeProject
    :: Text -- ^ 'dId'
    -> DescribeProject
describeProject pId_ = DescribeProject' {_dId = pId_}


-- | The ID of the project.
dId :: Lens' DescribeProject Text
dId = lens _dId (\ s a -> s{_dId = a})

instance AWSRequest DescribeProject where
        type Rs DescribeProject = DescribeProjectResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProjectResponse' <$>
                   (x .?> "arn") <*> (x .?> "projectTemplateId") <*>
                     (x .?> "name")
                     <*> (x .?> "id")
                     <*> (x .?> "stackId")
                     <*> (x .?> "clientRequestToken")
                     <*> (x .?> "createdTimeStamp")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProject where

instance NFData DescribeProject where

instance ToHeaders DescribeProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.DescribeProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProject where
        toJSON DescribeProject'{..}
          = object (catMaybes [Just ("id" .= _dId)])

instance ToPath DescribeProject where
        toPath = const "/"

instance ToQuery DescribeProject where
        toQuery = const mempty

-- | /See:/ 'describeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { _drsArn                :: !(Maybe Text)
  , _drsProjectTemplateId  :: !(Maybe Text)
  , _drsName               :: !(Maybe (Sensitive Text))
  , _drsId                 :: !(Maybe Text)
  , _drsStackId            :: !(Maybe Text)
  , _drsClientRequestToken :: !(Maybe Text)
  , _drsCreatedTimeStamp   :: !(Maybe POSIX)
  , _drsDescription        :: !(Maybe (Sensitive Text))
  , _drsResponseStatus     :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsArn' - The Amazon Resource Name (ARN) for the project.
--
-- * 'drsProjectTemplateId' - The ID for the AWS CodeStar project template used to create the project.
--
-- * 'drsName' - The display name for the project.
--
-- * 'drsId' - The ID of the project.
--
-- * 'drsStackId' - The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
--
-- * 'drsClientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation.
--
-- * 'drsCreatedTimeStamp' - The date and time the project was created, in timestamp format.
--
-- * 'drsDescription' - The description of the project, if any.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeProjectResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeProjectResponse
describeProjectResponse pResponseStatus_ =
  DescribeProjectResponse'
    { _drsArn = Nothing
    , _drsProjectTemplateId = Nothing
    , _drsName = Nothing
    , _drsId = Nothing
    , _drsStackId = Nothing
    , _drsClientRequestToken = Nothing
    , _drsCreatedTimeStamp = Nothing
    , _drsDescription = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) for the project.
drsArn :: Lens' DescribeProjectResponse (Maybe Text)
drsArn = lens _drsArn (\ s a -> s{_drsArn = a})

-- | The ID for the AWS CodeStar project template used to create the project.
drsProjectTemplateId :: Lens' DescribeProjectResponse (Maybe Text)
drsProjectTemplateId = lens _drsProjectTemplateId (\ s a -> s{_drsProjectTemplateId = a})

-- | The display name for the project.
drsName :: Lens' DescribeProjectResponse (Maybe Text)
drsName = lens _drsName (\ s a -> s{_drsName = a}) . mapping _Sensitive

-- | The ID of the project.
drsId :: Lens' DescribeProjectResponse (Maybe Text)
drsId = lens _drsId (\ s a -> s{_drsId = a})

-- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
drsStackId :: Lens' DescribeProjectResponse (Maybe Text)
drsStackId = lens _drsStackId (\ s a -> s{_drsStackId = a})

-- | A user- or system-generated token that identifies the entity that requested project creation.
drsClientRequestToken :: Lens' DescribeProjectResponse (Maybe Text)
drsClientRequestToken = lens _drsClientRequestToken (\ s a -> s{_drsClientRequestToken = a})

-- | The date and time the project was created, in timestamp format.
drsCreatedTimeStamp :: Lens' DescribeProjectResponse (Maybe UTCTime)
drsCreatedTimeStamp = lens _drsCreatedTimeStamp (\ s a -> s{_drsCreatedTimeStamp = a}) . mapping _Time

-- | The description of the project, if any.
drsDescription :: Lens' DescribeProjectResponse (Maybe Text)
drsDescription = lens _drsDescription (\ s a -> s{_drsDescription = a}) . mapping _Sensitive

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeProjectResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeProjectResponse where
