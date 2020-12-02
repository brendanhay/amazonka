{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a resource.
--
--
--
-- /See:/ 'resourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { _rARN :: !(Maybe Text),
    _rCreatedTime :: !(Maybe POSIX),
    _rName :: !(Maybe Text),
    _rId :: !(Maybe Text),
    _rDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rARN' - The ARN of the resource.
--
-- * 'rCreatedTime' - The creation time of the resource.
--
-- * 'rName' - The name of the resource.
--
-- * 'rId' - The identifier of the resource.
--
-- * 'rDescription' - The description of the resource.
resourceDetail ::
  ResourceDetail
resourceDetail =
  ResourceDetail'
    { _rARN = Nothing,
      _rCreatedTime = Nothing,
      _rName = Nothing,
      _rId = Nothing,
      _rDescription = Nothing
    }

-- | The ARN of the resource.
rARN :: Lens' ResourceDetail (Maybe Text)
rARN = lens _rARN (\s a -> s {_rARN = a})

-- | The creation time of the resource.
rCreatedTime :: Lens' ResourceDetail (Maybe UTCTime)
rCreatedTime = lens _rCreatedTime (\s a -> s {_rCreatedTime = a}) . mapping _Time

-- | The name of the resource.
rName :: Lens' ResourceDetail (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | The identifier of the resource.
rId :: Lens' ResourceDetail (Maybe Text)
rId = lens _rId (\s a -> s {_rId = a})

-- | The description of the resource.
rDescription :: Lens' ResourceDetail (Maybe Text)
rDescription = lens _rDescription (\s a -> s {_rDescription = a})

instance FromJSON ResourceDetail where
  parseJSON =
    withObject
      "ResourceDetail"
      ( \x ->
          ResourceDetail'
            <$> (x .:? "ARN")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Description")
      )

instance Hashable ResourceDetail

instance NFData ResourceDetail
