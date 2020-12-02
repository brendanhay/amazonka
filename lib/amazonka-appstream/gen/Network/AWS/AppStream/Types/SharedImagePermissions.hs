{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.SharedImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SharedImagePermissions where

import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the permissions that are available to the specified AWS account for a shared image.
--
--
--
-- /See:/ 'sharedImagePermissions' smart constructor.
data SharedImagePermissions = SharedImagePermissions'
  { _sipSharedAccountId ::
      !Text,
    _sipImagePermissions :: !ImagePermissions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SharedImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipSharedAccountId' - The 12-digit identifier of the AWS account with which the image is shared.
--
-- * 'sipImagePermissions' - Describes the permissions for a shared image.
sharedImagePermissions ::
  -- | 'sipSharedAccountId'
  Text ->
  -- | 'sipImagePermissions'
  ImagePermissions ->
  SharedImagePermissions
sharedImagePermissions pSharedAccountId_ pImagePermissions_ =
  SharedImagePermissions'
    { _sipSharedAccountId = pSharedAccountId_,
      _sipImagePermissions = pImagePermissions_
    }

-- | The 12-digit identifier of the AWS account with which the image is shared.
sipSharedAccountId :: Lens' SharedImagePermissions Text
sipSharedAccountId = lens _sipSharedAccountId (\s a -> s {_sipSharedAccountId = a})

-- | Describes the permissions for a shared image.
sipImagePermissions :: Lens' SharedImagePermissions ImagePermissions
sipImagePermissions = lens _sipImagePermissions (\s a -> s {_sipImagePermissions = a})

instance FromJSON SharedImagePermissions where
  parseJSON =
    withObject
      "SharedImagePermissions"
      ( \x ->
          SharedImagePermissions'
            <$> (x .: "sharedAccountId") <*> (x .: "imagePermissions")
      )

instance Hashable SharedImagePermissions

instance NFData SharedImagePermissions
