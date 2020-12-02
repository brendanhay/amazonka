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
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a notebook instance lifecycle configuration.
--
--
-- For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
--
module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
    (
    -- * Creating a Request
      describeNotebookInstanceLifecycleConfig
    , DescribeNotebookInstanceLifecycleConfig
    -- * Request Lenses
    , dNotebookInstanceLifecycleConfigName

    -- * Destructuring the Response
    , describeNotebookInstanceLifecycleConfigResponse
    , DescribeNotebookInstanceLifecycleConfigResponse
    -- * Response Lenses
    , dnilcrsCreationTime
    , dnilcrsOnCreate
    , dnilcrsLastModifiedTime
    , dnilcrsNotebookInstanceLifecycleConfigARN
    , dnilcrsOnStart
    , dnilcrsNotebookInstanceLifecycleConfigName
    , dnilcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeNotebookInstanceLifecycleConfig' smart constructor.
newtype DescribeNotebookInstanceLifecycleConfig = DescribeNotebookInstanceLifecycleConfig'
  { _dNotebookInstanceLifecycleConfigName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotebookInstanceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to describe.
describeNotebookInstanceLifecycleConfig
    :: Text -- ^ 'dNotebookInstanceLifecycleConfigName'
    -> DescribeNotebookInstanceLifecycleConfig
describeNotebookInstanceLifecycleConfig pNotebookInstanceLifecycleConfigName_ =
  DescribeNotebookInstanceLifecycleConfig'
    { _dNotebookInstanceLifecycleConfigName =
        pNotebookInstanceLifecycleConfigName_
    }


-- | The name of the lifecycle configuration to describe.
dNotebookInstanceLifecycleConfigName :: Lens' DescribeNotebookInstanceLifecycleConfig Text
dNotebookInstanceLifecycleConfigName = lens _dNotebookInstanceLifecycleConfigName (\ s a -> s{_dNotebookInstanceLifecycleConfigName = a})

instance AWSRequest
           DescribeNotebookInstanceLifecycleConfig
         where
        type Rs DescribeNotebookInstanceLifecycleConfig =
             DescribeNotebookInstanceLifecycleConfigResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNotebookInstanceLifecycleConfigResponse' <$>
                   (x .?> "CreationTime") <*>
                     (x .?> "OnCreate" .!@ mempty)
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "NotebookInstanceLifecycleConfigArn")
                     <*> (x .?> "OnStart" .!@ mempty)
                     <*> (x .?> "NotebookInstanceLifecycleConfigName")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeNotebookInstanceLifecycleConfig
         where

instance NFData
           DescribeNotebookInstanceLifecycleConfig
         where

instance ToHeaders
           DescribeNotebookInstanceLifecycleConfig
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeNotebookInstanceLifecycleConfig"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeNotebookInstanceLifecycleConfig
         where
        toJSON DescribeNotebookInstanceLifecycleConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceLifecycleConfigName" .=
                       _dNotebookInstanceLifecycleConfigName)])

instance ToPath
           DescribeNotebookInstanceLifecycleConfig
         where
        toPath = const "/"

instance ToQuery
           DescribeNotebookInstanceLifecycleConfig
         where
        toQuery = const mempty

-- | /See:/ 'describeNotebookInstanceLifecycleConfigResponse' smart constructor.
data DescribeNotebookInstanceLifecycleConfigResponse = DescribeNotebookInstanceLifecycleConfigResponse'
  { _dnilcrsCreationTime :: !(Maybe POSIX)
  , _dnilcrsOnCreate :: !(Maybe [NotebookInstanceLifecycleHook])
  , _dnilcrsLastModifiedTime :: !(Maybe POSIX)
  , _dnilcrsNotebookInstanceLifecycleConfigARN :: !(Maybe Text)
  , _dnilcrsOnStart :: !(Maybe [NotebookInstanceLifecycleHook])
  , _dnilcrsNotebookInstanceLifecycleConfigName :: !(Maybe Text)
  , _dnilcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotebookInstanceLifecycleConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnilcrsCreationTime' - A timestamp that tells when the lifecycle configuration was created.
--
-- * 'dnilcrsOnCreate' - The shell script that runs only once, when you create a notebook instance.
--
-- * 'dnilcrsLastModifiedTime' - A timestamp that tells when the lifecycle configuration was last modified.
--
-- * 'dnilcrsNotebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- * 'dnilcrsOnStart' - The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
--
-- * 'dnilcrsNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- * 'dnilcrsResponseStatus' - -- | The response status code.
describeNotebookInstanceLifecycleConfigResponse
    :: Int -- ^ 'dnilcrsResponseStatus'
    -> DescribeNotebookInstanceLifecycleConfigResponse
describeNotebookInstanceLifecycleConfigResponse pResponseStatus_ =
  DescribeNotebookInstanceLifecycleConfigResponse'
    { _dnilcrsCreationTime = Nothing
    , _dnilcrsOnCreate = Nothing
    , _dnilcrsLastModifiedTime = Nothing
    , _dnilcrsNotebookInstanceLifecycleConfigARN = Nothing
    , _dnilcrsOnStart = Nothing
    , _dnilcrsNotebookInstanceLifecycleConfigName = Nothing
    , _dnilcrsResponseStatus = pResponseStatus_
    }


-- | A timestamp that tells when the lifecycle configuration was created.
dnilcrsCreationTime :: Lens' DescribeNotebookInstanceLifecycleConfigResponse (Maybe UTCTime)
dnilcrsCreationTime = lens _dnilcrsCreationTime (\ s a -> s{_dnilcrsCreationTime = a}) . mapping _Time

-- | The shell script that runs only once, when you create a notebook instance.
dnilcrsOnCreate :: Lens' DescribeNotebookInstanceLifecycleConfigResponse [NotebookInstanceLifecycleHook]
dnilcrsOnCreate = lens _dnilcrsOnCreate (\ s a -> s{_dnilcrsOnCreate = a}) . _Default . _Coerce

-- | A timestamp that tells when the lifecycle configuration was last modified.
dnilcrsLastModifiedTime :: Lens' DescribeNotebookInstanceLifecycleConfigResponse (Maybe UTCTime)
dnilcrsLastModifiedTime = lens _dnilcrsLastModifiedTime (\ s a -> s{_dnilcrsLastModifiedTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
dnilcrsNotebookInstanceLifecycleConfigARN :: Lens' DescribeNotebookInstanceLifecycleConfigResponse (Maybe Text)
dnilcrsNotebookInstanceLifecycleConfigARN = lens _dnilcrsNotebookInstanceLifecycleConfigARN (\ s a -> s{_dnilcrsNotebookInstanceLifecycleConfigARN = a})

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
dnilcrsOnStart :: Lens' DescribeNotebookInstanceLifecycleConfigResponse [NotebookInstanceLifecycleHook]
dnilcrsOnStart = lens _dnilcrsOnStart (\ s a -> s{_dnilcrsOnStart = a}) . _Default . _Coerce

-- | The name of the lifecycle configuration.
dnilcrsNotebookInstanceLifecycleConfigName :: Lens' DescribeNotebookInstanceLifecycleConfigResponse (Maybe Text)
dnilcrsNotebookInstanceLifecycleConfigName = lens _dnilcrsNotebookInstanceLifecycleConfigName (\ s a -> s{_dnilcrsNotebookInstanceLifecycleConfigName = a})

-- | -- | The response status code.
dnilcrsResponseStatus :: Lens' DescribeNotebookInstanceLifecycleConfigResponse Int
dnilcrsResponseStatus = lens _dnilcrsResponseStatus (\ s a -> s{_dnilcrsResponseStatus = a})

instance NFData
           DescribeNotebookInstanceLifecycleConfigResponse
         where
