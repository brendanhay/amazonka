{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectAttributes' response operation.
--
--
--
-- /See:/ 'batchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { _bNextToken ::
      !(Maybe Text),
    _bAttributes ::
      !( Maybe
           [AttributeKeyAndValue]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bNextToken' - The pagination token.
--
-- * 'bAttributes' - The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
batchListObjectAttributesResponse ::
  BatchListObjectAttributesResponse
batchListObjectAttributesResponse =
  BatchListObjectAttributesResponse'
    { _bNextToken = Nothing,
      _bAttributes = Nothing
    }

-- | The pagination token.
bNextToken :: Lens' BatchListObjectAttributesResponse (Maybe Text)
bNextToken = lens _bNextToken (\s a -> s {_bNextToken = a})

-- | The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
bAttributes :: Lens' BatchListObjectAttributesResponse [AttributeKeyAndValue]
bAttributes = lens _bAttributes (\s a -> s {_bAttributes = a}) . _Default . _Coerce

instance FromJSON BatchListObjectAttributesResponse where
  parseJSON =
    withObject
      "BatchListObjectAttributesResponse"
      ( \x ->
          BatchListObjectAttributesResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable BatchListObjectAttributesResponse

instance NFData BatchListObjectAttributesResponse
