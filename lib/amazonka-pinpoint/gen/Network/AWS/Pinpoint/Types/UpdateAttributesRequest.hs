{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.UpdateAttributesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.UpdateAttributesRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies one or more attributes to remove from all the endpoints that are associated with an application.
--
--
--
-- /See:/ 'updateAttributesRequest' smart constructor.
newtype UpdateAttributesRequest = UpdateAttributesRequest'
  { _uarBlacklist ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAttributesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarBlacklist' - An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
updateAttributesRequest ::
  UpdateAttributesRequest
updateAttributesRequest =
  UpdateAttributesRequest' {_uarBlacklist = Nothing}

-- | An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
uarBlacklist :: Lens' UpdateAttributesRequest [Text]
uarBlacklist = lens _uarBlacklist (\s a -> s {_uarBlacklist = a}) . _Default . _Coerce

instance Hashable UpdateAttributesRequest

instance NFData UpdateAttributesRequest

instance ToJSON UpdateAttributesRequest where
  toJSON UpdateAttributesRequest' {..} =
    object (catMaybes [("Blacklist" .=) <$> _uarBlacklist])
