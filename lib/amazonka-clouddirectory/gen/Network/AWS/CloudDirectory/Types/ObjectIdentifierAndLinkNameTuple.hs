{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A pair of ObjectIdentifier and LinkName.
--
--
--
-- /See:/ 'objectIdentifierAndLinkNameTuple' smart constructor.
data ObjectIdentifierAndLinkNameTuple = ObjectIdentifierAndLinkNameTuple'
  { _oialntObjectIdentifier ::
      !(Maybe Text),
    _oialntLinkName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectIdentifierAndLinkNameTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oialntObjectIdentifier' - The ID that is associated with the object.
--
-- * 'oialntLinkName' - The name of the link between the parent and the child object.
objectIdentifierAndLinkNameTuple ::
  ObjectIdentifierAndLinkNameTuple
objectIdentifierAndLinkNameTuple =
  ObjectIdentifierAndLinkNameTuple'
    { _oialntObjectIdentifier =
        Nothing,
      _oialntLinkName = Nothing
    }

-- | The ID that is associated with the object.
oialntObjectIdentifier :: Lens' ObjectIdentifierAndLinkNameTuple (Maybe Text)
oialntObjectIdentifier = lens _oialntObjectIdentifier (\s a -> s {_oialntObjectIdentifier = a})

-- | The name of the link between the parent and the child object.
oialntLinkName :: Lens' ObjectIdentifierAndLinkNameTuple (Maybe Text)
oialntLinkName = lens _oialntLinkName (\s a -> s {_oialntLinkName = a})

instance FromJSON ObjectIdentifierAndLinkNameTuple where
  parseJSON =
    withObject
      "ObjectIdentifierAndLinkNameTuple"
      ( \x ->
          ObjectIdentifierAndLinkNameTuple'
            <$> (x .:? "ObjectIdentifier") <*> (x .:? "LinkName")
      )

instance Hashable ObjectIdentifierAndLinkNameTuple

instance NFData ObjectIdentifierAndLinkNameTuple
