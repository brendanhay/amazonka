{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'AttachTypedLink' response operation.
--
--
--
-- /See:/ 'batchAttachTypedLinkResponse' smart constructor.
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse'
  { _batlTypedLinkSpecifier ::
      Maybe TypedLinkSpecifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachTypedLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batlTypedLinkSpecifier' - Returns a typed link specifier as output.
batchAttachTypedLinkResponse ::
  BatchAttachTypedLinkResponse
batchAttachTypedLinkResponse =
  BatchAttachTypedLinkResponse' {_batlTypedLinkSpecifier = Nothing}

-- | Returns a typed link specifier as output.
batlTypedLinkSpecifier :: Lens' BatchAttachTypedLinkResponse (Maybe TypedLinkSpecifier)
batlTypedLinkSpecifier = lens _batlTypedLinkSpecifier (\s a -> s {_batlTypedLinkSpecifier = a})

instance FromJSON BatchAttachTypedLinkResponse where
  parseJSON =
    withObject
      "BatchAttachTypedLinkResponse"
      ( \x ->
          BatchAttachTypedLinkResponse' <$> (x .:? "TypedLinkSpecifier")
      )

instance Hashable BatchAttachTypedLinkResponse

instance NFData BatchAttachTypedLinkResponse
