{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse where

import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'batchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { _blopNextToken ::
      !(Maybe Text),
    _blopParentLinks ::
      !( Maybe
           [ObjectIdentifierAndLinkNameTuple]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectParentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blopNextToken' - Undocumented member.
--
-- * 'blopParentLinks' - Undocumented member.
batchListObjectParentsResponse ::
  BatchListObjectParentsResponse
batchListObjectParentsResponse =
  BatchListObjectParentsResponse'
    { _blopNextToken = Nothing,
      _blopParentLinks = Nothing
    }

-- | Undocumented member.
blopNextToken :: Lens' BatchListObjectParentsResponse (Maybe Text)
blopNextToken = lens _blopNextToken (\s a -> s {_blopNextToken = a})

-- | Undocumented member.
blopParentLinks :: Lens' BatchListObjectParentsResponse [ObjectIdentifierAndLinkNameTuple]
blopParentLinks = lens _blopParentLinks (\s a -> s {_blopParentLinks = a}) . _Default . _Coerce

instance FromJSON BatchListObjectParentsResponse where
  parseJSON =
    withObject
      "BatchListObjectParentsResponse"
      ( \x ->
          BatchListObjectParentsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "ParentLinks" .!= mempty)
      )

instance Hashable BatchListObjectParentsResponse

instance NFData BatchListObjectParentsResponse
