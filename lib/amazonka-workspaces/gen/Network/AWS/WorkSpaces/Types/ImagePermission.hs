{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ImagePermission where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the AWS accounts that have been granted permission to use a shared image. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
--
--
--
-- /See:/ 'imagePermission' smart constructor.
newtype ImagePermission = ImagePermission'
  { _ipSharedAccountId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImagePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipSharedAccountId' - The identifier of the AWS account that an image has been shared with.
imagePermission ::
  ImagePermission
imagePermission = ImagePermission' {_ipSharedAccountId = Nothing}

-- | The identifier of the AWS account that an image has been shared with.
ipSharedAccountId :: Lens' ImagePermission (Maybe Text)
ipSharedAccountId = lens _ipSharedAccountId (\s a -> s {_ipSharedAccountId = a})

instance FromJSON ImagePermission where
  parseJSON =
    withObject
      "ImagePermission"
      (\x -> ImagePermission' <$> (x .:? "SharedAccountId"))

instance Hashable ImagePermission

instance NFData ImagePermission
