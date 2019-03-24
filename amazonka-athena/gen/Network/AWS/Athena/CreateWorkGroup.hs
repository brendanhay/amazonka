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
-- Module      : Network.AWS.Athena.CreateWorkGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workgroup with the specified name.
--
--
module Network.AWS.Athena.CreateWorkGroup
    (
    -- * Creating a Request
      createWorkGroup
    , CreateWorkGroup
    -- * Request Lenses
    , cwgConfiguration
    , cwgDescription
    , cwgTags
    , cwgName

    -- * Destructuring the Response
    , createWorkGroupResponse
    , CreateWorkGroupResponse
    -- * Response Lenses
    , cwgrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createWorkGroup' smart constructor.
data CreateWorkGroup = CreateWorkGroup'
  { _cwgConfiguration :: !(Maybe WorkGroupConfiguration)
  , _cwgDescription   :: !(Maybe Text)
  , _cwgTags          :: !(Maybe [Tag])
  , _cwgName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWorkGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwgConfiguration' - The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- * 'cwgDescription' - The workgroup description.
--
-- * 'cwgTags' - One or more tags, separated by commas, that you want to attach to the workgroup as you create it.
--
-- * 'cwgName' - The workgroup name.
createWorkGroup
    :: Text -- ^ 'cwgName'
    -> CreateWorkGroup
createWorkGroup pName_ =
  CreateWorkGroup'
    { _cwgConfiguration = Nothing
    , _cwgDescription = Nothing
    , _cwgTags = Nothing
    , _cwgName = pName_
    }


-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
cwgConfiguration :: Lens' CreateWorkGroup (Maybe WorkGroupConfiguration)
cwgConfiguration = lens _cwgConfiguration (\ s a -> s{_cwgConfiguration = a})

-- | The workgroup description.
cwgDescription :: Lens' CreateWorkGroup (Maybe Text)
cwgDescription = lens _cwgDescription (\ s a -> s{_cwgDescription = a})

-- | One or more tags, separated by commas, that you want to attach to the workgroup as you create it.
cwgTags :: Lens' CreateWorkGroup [Tag]
cwgTags = lens _cwgTags (\ s a -> s{_cwgTags = a}) . _Default . _Coerce

-- | The workgroup name.
cwgName :: Lens' CreateWorkGroup Text
cwgName = lens _cwgName (\ s a -> s{_cwgName = a})

instance AWSRequest CreateWorkGroup where
        type Rs CreateWorkGroup = CreateWorkGroupResponse
        request = postJSON athena
        response
          = receiveEmpty
              (\ s h x ->
                 CreateWorkGroupResponse' <$> (pure (fromEnum s)))

instance Hashable CreateWorkGroup where

instance NFData CreateWorkGroup where

instance ToHeaders CreateWorkGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.CreateWorkGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateWorkGroup where
        toJSON CreateWorkGroup'{..}
          = object
              (catMaybes
                 [("Configuration" .=) <$> _cwgConfiguration,
                  ("Description" .=) <$> _cwgDescription,
                  ("Tags" .=) <$> _cwgTags, Just ("Name" .= _cwgName)])

instance ToPath CreateWorkGroup where
        toPath = const "/"

instance ToQuery CreateWorkGroup where
        toQuery = const mempty

-- | /See:/ 'createWorkGroupResponse' smart constructor.
newtype CreateWorkGroupResponse = CreateWorkGroupResponse'
  { _cwgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWorkGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwgrsResponseStatus' - -- | The response status code.
createWorkGroupResponse
    :: Int -- ^ 'cwgrsResponseStatus'
    -> CreateWorkGroupResponse
createWorkGroupResponse pResponseStatus_ =
  CreateWorkGroupResponse' {_cwgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cwgrsResponseStatus :: Lens' CreateWorkGroupResponse Int
cwgrsResponseStatus = lens _cwgrsResponseStatus (\ s a -> s{_cwgrsResponseStatus = a})

instance NFData CreateWorkGroupResponse where
