{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Grantee
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Grantee where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Type

-- | Container for the person being granted permissions.
--
--
--
-- /See:/ 'grantee' smart constructor.
data Grantee = Grantee'
  { _gURI :: !(Maybe Text),
    _gEmailAddress :: !(Maybe Text),
    _gDisplayName :: !(Maybe Text),
    _gId :: !(Maybe Text),
    _gType :: !Type
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Grantee' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gURI' - URI of the grantee group.
--
-- * 'gEmailAddress' - Email address of the grantee.
--
-- * 'gDisplayName' - Screen name of the grantee.
--
-- * 'gId' - The canonical user ID of the grantee.
--
-- * 'gType' - Type of grantee
grantee ::
  -- | 'gType'
  Type ->
  Grantee
grantee pType_ =
  Grantee'
    { _gURI = Nothing,
      _gEmailAddress = Nothing,
      _gDisplayName = Nothing,
      _gId = Nothing,
      _gType = pType_
    }

-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\s a -> s {_gURI = a})

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\s a -> s {_gEmailAddress = a})

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\s a -> s {_gDisplayName = a})

-- | The canonical user ID of the grantee.
gId :: Lens' Grantee (Maybe Text)
gId = lens _gId (\s a -> s {_gId = a})

-- | Type of grantee
gType :: Lens' Grantee Type
gType = lens _gType (\s a -> s {_gType = a})

instance FromXML Grantee where
  parseXML x =
    Grantee'
      <$> (x .@? "URI")
      <*> (x .@? "EmailAddress")
      <*> (x .@? "DisplayName")
      <*> (x .@? "ID")
      <*> (x .@ "xsi:type")

instance Hashable Grantee

instance NFData Grantee

instance ToXML Grantee where
  toXML Grantee' {..} =
    mconcat
      [ "URI" @= _gURI,
        "EmailAddress" @= _gEmailAddress,
        "DisplayName" @= _gDisplayName,
        "ID" @= _gId,
        "xsi:type" @@= _gType
      ]
