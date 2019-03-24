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
-- Module      : Network.AWS.SageMaker.DescribeCodeRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about the specified Git repository.
--
--
module Network.AWS.SageMaker.DescribeCodeRepository
    (
    -- * Creating a Request
      describeCodeRepository
    , DescribeCodeRepository
    -- * Request Lenses
    , dcrCodeRepositoryName

    -- * Destructuring the Response
    , describeCodeRepositoryResponse
    , DescribeCodeRepositoryResponse
    -- * Response Lenses
    , dcrrsGitConfig
    , dcrrsResponseStatus
    , dcrrsCodeRepositoryName
    , dcrrsCodeRepositoryARN
    , dcrrsCreationTime
    , dcrrsLastModifiedTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeCodeRepository' smart constructor.
newtype DescribeCodeRepository = DescribeCodeRepository'
  { _dcrCodeRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCodeRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrCodeRepositoryName' - The name of the Git repository to describe.
describeCodeRepository
    :: Text -- ^ 'dcrCodeRepositoryName'
    -> DescribeCodeRepository
describeCodeRepository pCodeRepositoryName_ =
  DescribeCodeRepository' {_dcrCodeRepositoryName = pCodeRepositoryName_}


-- | The name of the Git repository to describe.
dcrCodeRepositoryName :: Lens' DescribeCodeRepository Text
dcrCodeRepositoryName = lens _dcrCodeRepositoryName (\ s a -> s{_dcrCodeRepositoryName = a})

instance AWSRequest DescribeCodeRepository where
        type Rs DescribeCodeRepository =
             DescribeCodeRepositoryResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCodeRepositoryResponse' <$>
                   (x .?> "GitConfig") <*> (pure (fromEnum s)) <*>
                     (x .:> "CodeRepositoryName")
                     <*> (x .:> "CodeRepositoryArn")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "LastModifiedTime"))

instance Hashable DescribeCodeRepository where

instance NFData DescribeCodeRepository where

instance ToHeaders DescribeCodeRepository where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeCodeRepository" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCodeRepository where
        toJSON DescribeCodeRepository'{..}
          = object
              (catMaybes
                 [Just
                    ("CodeRepositoryName" .= _dcrCodeRepositoryName)])

instance ToPath DescribeCodeRepository where
        toPath = const "/"

instance ToQuery DescribeCodeRepository where
        toQuery = const mempty

-- | /See:/ 'describeCodeRepositoryResponse' smart constructor.
data DescribeCodeRepositoryResponse = DescribeCodeRepositoryResponse'
  { _dcrrsGitConfig          :: !(Maybe GitConfig)
  , _dcrrsResponseStatus     :: !Int
  , _dcrrsCodeRepositoryName :: !Text
  , _dcrrsCodeRepositoryARN  :: !Text
  , _dcrrsCreationTime       :: !POSIX
  , _dcrrsLastModifiedTime   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrrsGitConfig' - Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- * 'dcrrsResponseStatus' - -- | The response status code.
--
-- * 'dcrrsCodeRepositoryName' - The name of the Git repository.
--
-- * 'dcrrsCodeRepositoryARN' - The Amazon Resource Name (ARN) of the Git repository.
--
-- * 'dcrrsCreationTime' - The date and time that the repository was created.
--
-- * 'dcrrsLastModifiedTime' - The date and time that the repository was last changed.
describeCodeRepositoryResponse
    :: Int -- ^ 'dcrrsResponseStatus'
    -> Text -- ^ 'dcrrsCodeRepositoryName'
    -> Text -- ^ 'dcrrsCodeRepositoryARN'
    -> UTCTime -- ^ 'dcrrsCreationTime'
    -> UTCTime -- ^ 'dcrrsLastModifiedTime'
    -> DescribeCodeRepositoryResponse
describeCodeRepositoryResponse pResponseStatus_ pCodeRepositoryName_ pCodeRepositoryARN_ pCreationTime_ pLastModifiedTime_ =
  DescribeCodeRepositoryResponse'
    { _dcrrsGitConfig = Nothing
    , _dcrrsResponseStatus = pResponseStatus_
    , _dcrrsCodeRepositoryName = pCodeRepositoryName_
    , _dcrrsCodeRepositoryARN = pCodeRepositoryARN_
    , _dcrrsCreationTime = _Time # pCreationTime_
    , _dcrrsLastModifiedTime = _Time # pLastModifiedTime_
    }


-- | Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
dcrrsGitConfig :: Lens' DescribeCodeRepositoryResponse (Maybe GitConfig)
dcrrsGitConfig = lens _dcrrsGitConfig (\ s a -> s{_dcrrsGitConfig = a})

-- | -- | The response status code.
dcrrsResponseStatus :: Lens' DescribeCodeRepositoryResponse Int
dcrrsResponseStatus = lens _dcrrsResponseStatus (\ s a -> s{_dcrrsResponseStatus = a})

-- | The name of the Git repository.
dcrrsCodeRepositoryName :: Lens' DescribeCodeRepositoryResponse Text
dcrrsCodeRepositoryName = lens _dcrrsCodeRepositoryName (\ s a -> s{_dcrrsCodeRepositoryName = a})

-- | The Amazon Resource Name (ARN) of the Git repository.
dcrrsCodeRepositoryARN :: Lens' DescribeCodeRepositoryResponse Text
dcrrsCodeRepositoryARN = lens _dcrrsCodeRepositoryARN (\ s a -> s{_dcrrsCodeRepositoryARN = a})

-- | The date and time that the repository was created.
dcrrsCreationTime :: Lens' DescribeCodeRepositoryResponse UTCTime
dcrrsCreationTime = lens _dcrrsCreationTime (\ s a -> s{_dcrrsCreationTime = a}) . _Time

-- | The date and time that the repository was last changed.
dcrrsLastModifiedTime :: Lens' DescribeCodeRepositoryResponse UTCTime
dcrrsLastModifiedTime = lens _dcrrsLastModifiedTime (\ s a -> s{_dcrrsLastModifiedTime = a}) . _Time

instance NFData DescribeCodeRepositoryResponse where
