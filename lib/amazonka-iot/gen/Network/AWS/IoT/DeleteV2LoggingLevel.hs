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
-- Module      : Network.AWS.IoT.DeleteV2LoggingLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logging level.
--
--
module Network.AWS.IoT.DeleteV2LoggingLevel
    (
    -- * Creating a Request
      deleteV2LoggingLevel
    , DeleteV2LoggingLevel
    -- * Request Lenses
    , dvllTargetType
    , dvllTargetName

    -- * Destructuring the Response
    , deleteV2LoggingLevelResponse
    , DeleteV2LoggingLevelResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteV2LoggingLevel' smart constructor.
data DeleteV2LoggingLevel = DeleteV2LoggingLevel'
  { _dvllTargetType :: !LogTargetType
  , _dvllTargetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteV2LoggingLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvllTargetType' - The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- * 'dvllTargetName' - The name of the resource for which you are configuring logging.
deleteV2LoggingLevel
    :: LogTargetType -- ^ 'dvllTargetType'
    -> Text -- ^ 'dvllTargetName'
    -> DeleteV2LoggingLevel
deleteV2LoggingLevel pTargetType_ pTargetName_ =
  DeleteV2LoggingLevel'
    {_dvllTargetType = pTargetType_, _dvllTargetName = pTargetName_}


-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
dvllTargetType :: Lens' DeleteV2LoggingLevel LogTargetType
dvllTargetType = lens _dvllTargetType (\ s a -> s{_dvllTargetType = a})

-- | The name of the resource for which you are configuring logging.
dvllTargetName :: Lens' DeleteV2LoggingLevel Text
dvllTargetName = lens _dvllTargetName (\ s a -> s{_dvllTargetName = a})

instance AWSRequest DeleteV2LoggingLevel where
        type Rs DeleteV2LoggingLevel =
             DeleteV2LoggingLevelResponse
        request = delete ioT
        response = receiveNull DeleteV2LoggingLevelResponse'

instance Hashable DeleteV2LoggingLevel where

instance NFData DeleteV2LoggingLevel where

instance ToHeaders DeleteV2LoggingLevel where
        toHeaders = const mempty

instance ToPath DeleteV2LoggingLevel where
        toPath = const "/v2LoggingLevel"

instance ToQuery DeleteV2LoggingLevel where
        toQuery DeleteV2LoggingLevel'{..}
          = mconcat
              ["targetType" =: _dvllTargetType,
               "targetName" =: _dvllTargetName]

-- | /See:/ 'deleteV2LoggingLevelResponse' smart constructor.
data DeleteV2LoggingLevelResponse =
  DeleteV2LoggingLevelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteV2LoggingLevelResponse' with the minimum fields required to make a request.
--
deleteV2LoggingLevelResponse
    :: DeleteV2LoggingLevelResponse
deleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'


instance NFData DeleteV2LoggingLevelResponse where
