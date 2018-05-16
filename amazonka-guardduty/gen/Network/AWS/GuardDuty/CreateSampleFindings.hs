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
-- Module      : Network.AWS.GuardDuty.CreateSampleFindings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates example findings of types specified by the list of finding types. If 'NULL' is specified for findingTypes, the API generates example findings of all supported finding types.
module Network.AWS.GuardDuty.CreateSampleFindings
    (
    -- * Creating a Request
      createSampleFindings
    , CreateSampleFindings
    -- * Request Lenses
    , csfFindingTypes
    , csfDetectorId

    -- * Destructuring the Response
    , createSampleFindingsResponse
    , CreateSampleFindingsResponse
    -- * Response Lenses
    , csfrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateSampleFindings request body.
--
-- /See:/ 'createSampleFindings' smart constructor.
data CreateSampleFindings = CreateSampleFindings'
  { _csfFindingTypes :: !(Maybe [Text])
  , _csfDetectorId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSampleFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfFindingTypes' - Types of sample findings that you want to generate.
--
-- * 'csfDetectorId' - The ID of the detector to create sample findings for.
createSampleFindings
    :: Text -- ^ 'csfDetectorId'
    -> CreateSampleFindings
createSampleFindings pDetectorId_ =
  CreateSampleFindings'
    {_csfFindingTypes = Nothing, _csfDetectorId = pDetectorId_}


-- | Types of sample findings that you want to generate.
csfFindingTypes :: Lens' CreateSampleFindings [Text]
csfFindingTypes = lens _csfFindingTypes (\ s a -> s{_csfFindingTypes = a}) . _Default . _Coerce

-- | The ID of the detector to create sample findings for.
csfDetectorId :: Lens' CreateSampleFindings Text
csfDetectorId = lens _csfDetectorId (\ s a -> s{_csfDetectorId = a})

instance AWSRequest CreateSampleFindings where
        type Rs CreateSampleFindings =
             CreateSampleFindingsResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 CreateSampleFindingsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateSampleFindings where

instance NFData CreateSampleFindings where

instance ToHeaders CreateSampleFindings where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSampleFindings where
        toJSON CreateSampleFindings'{..}
          = object
              (catMaybes
                 [("findingTypes" .=) <$> _csfFindingTypes])

instance ToPath CreateSampleFindings where
        toPath CreateSampleFindings'{..}
          = mconcat
              ["/detector/", toBS _csfDetectorId,
               "/findings/create"]

instance ToQuery CreateSampleFindings where
        toQuery = const mempty

-- | /See:/ 'createSampleFindingsResponse' smart constructor.
newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse'
  { _csfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSampleFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfrsResponseStatus' - -- | The response status code.
createSampleFindingsResponse
    :: Int -- ^ 'csfrsResponseStatus'
    -> CreateSampleFindingsResponse
createSampleFindingsResponse pResponseStatus_ =
  CreateSampleFindingsResponse' {_csfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
csfrsResponseStatus :: Lens' CreateSampleFindingsResponse Int
csfrsResponseStatus = lens _csfrsResponseStatus (\ s a -> s{_csfrsResponseStatus = a})

instance NFData CreateSampleFindingsResponse where
