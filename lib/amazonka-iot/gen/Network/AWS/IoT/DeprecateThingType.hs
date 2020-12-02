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
-- Module      : Network.AWS.IoT.DeprecateThingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates a thing type. You can not associate new things with deprecated thing type.
--
--
module Network.AWS.IoT.DeprecateThingType
    (
    -- * Creating a Request
      deprecateThingType
    , DeprecateThingType
    -- * Request Lenses
    , depUndoDeprecate
    , depThingTypeName

    -- * Destructuring the Response
    , deprecateThingTypeResponse
    , DeprecateThingTypeResponse
    -- * Response Lenses
    , deprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeprecateThingType operation.
--
--
--
-- /See:/ 'deprecateThingType' smart constructor.
data DeprecateThingType = DeprecateThingType'
  { _depUndoDeprecate :: !(Maybe Bool)
  , _depThingTypeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depUndoDeprecate' - Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
--
-- * 'depThingTypeName' - The name of the thing type to deprecate.
deprecateThingType
    :: Text -- ^ 'depThingTypeName'
    -> DeprecateThingType
deprecateThingType pThingTypeName_ =
  DeprecateThingType'
    {_depUndoDeprecate = Nothing, _depThingTypeName = pThingTypeName_}


-- | Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
depUndoDeprecate :: Lens' DeprecateThingType (Maybe Bool)
depUndoDeprecate = lens _depUndoDeprecate (\ s a -> s{_depUndoDeprecate = a})

-- | The name of the thing type to deprecate.
depThingTypeName :: Lens' DeprecateThingType Text
depThingTypeName = lens _depThingTypeName (\ s a -> s{_depThingTypeName = a})

instance AWSRequest DeprecateThingType where
        type Rs DeprecateThingType =
             DeprecateThingTypeResponse
        request = postJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeprecateThingTypeResponse' <$> (pure (fromEnum s)))

instance Hashable DeprecateThingType where

instance NFData DeprecateThingType where

instance ToHeaders DeprecateThingType where
        toHeaders = const mempty

instance ToJSON DeprecateThingType where
        toJSON DeprecateThingType'{..}
          = object
              (catMaybes
                 [("undoDeprecate" .=) <$> _depUndoDeprecate])

instance ToPath DeprecateThingType where
        toPath DeprecateThingType'{..}
          = mconcat
              ["/thing-types/", toBS _depThingTypeName,
               "/deprecate"]

instance ToQuery DeprecateThingType where
        toQuery = const mempty

-- | The output for the DeprecateThingType operation.
--
--
--
-- /See:/ 'deprecateThingTypeResponse' smart constructor.
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse'
  { _deprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deprsResponseStatus' - -- | The response status code.
deprecateThingTypeResponse
    :: Int -- ^ 'deprsResponseStatus'
    -> DeprecateThingTypeResponse
deprecateThingTypeResponse pResponseStatus_ =
  DeprecateThingTypeResponse' {_deprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
deprsResponseStatus :: Lens' DeprecateThingTypeResponse Int
deprsResponseStatus = lens _deprsResponseStatus (\ s a -> s{_deprsResponseStatus = a})

instance NFData DeprecateThingTypeResponse where
