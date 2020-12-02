{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'GetLinkAttributes' response operation.
--
--
--
-- /See:/ 'batchGetLinkAttributesResponse' smart constructor.
newtype BatchGetLinkAttributesResponse = BatchGetLinkAttributesResponse'
  { _bglaAttributes ::
      Maybe
        [AttributeKeyAndValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetLinkAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bglaAttributes' - The attributes that are associated with the typed link.
batchGetLinkAttributesResponse ::
  BatchGetLinkAttributesResponse
batchGetLinkAttributesResponse =
  BatchGetLinkAttributesResponse' {_bglaAttributes = Nothing}

-- | The attributes that are associated with the typed link.
bglaAttributes :: Lens' BatchGetLinkAttributesResponse [AttributeKeyAndValue]
bglaAttributes = lens _bglaAttributes (\s a -> s {_bglaAttributes = a}) . _Default . _Coerce

instance FromJSON BatchGetLinkAttributesResponse where
  parseJSON =
    withObject
      "BatchGetLinkAttributesResponse"
      ( \x ->
          BatchGetLinkAttributesResponse'
            <$> (x .:? "Attributes" .!= mempty)
      )

instance Hashable BatchGetLinkAttributesResponse

instance NFData BatchGetLinkAttributesResponse
