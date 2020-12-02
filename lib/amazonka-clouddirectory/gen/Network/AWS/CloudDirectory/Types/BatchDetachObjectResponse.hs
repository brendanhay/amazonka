{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DetachObject' response operation.
--
--
--
-- /See:/ 'batchDetachObjectResponse' smart constructor.
newtype BatchDetachObjectResponse = BatchDetachObjectResponse'
  { _bdoDetachedObjectIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoDetachedObjectIdentifier' - The @ObjectIdentifier@ of the detached object.
batchDetachObjectResponse ::
  BatchDetachObjectResponse
batchDetachObjectResponse =
  BatchDetachObjectResponse'
    { _bdoDetachedObjectIdentifier =
        Nothing
    }

-- | The @ObjectIdentifier@ of the detached object.
bdoDetachedObjectIdentifier :: Lens' BatchDetachObjectResponse (Maybe Text)
bdoDetachedObjectIdentifier = lens _bdoDetachedObjectIdentifier (\s a -> s {_bdoDetachedObjectIdentifier = a})

instance FromJSON BatchDetachObjectResponse where
  parseJSON =
    withObject
      "BatchDetachObjectResponse"
      ( \x ->
          BatchDetachObjectResponse' <$> (x .:? "detachedObjectIdentifier")
      )

instance Hashable BatchDetachObjectResponse

instance NFData BatchDetachObjectResponse
