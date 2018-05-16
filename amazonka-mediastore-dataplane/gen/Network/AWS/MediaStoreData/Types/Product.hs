{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStoreData.Types.Product where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types.Sum
import Network.AWS.Prelude

-- | A metadata entry for a folder or object.
--
--
--
-- /See:/ 'item' smart constructor.
data Item = Item'
  { _iETag          :: !(Maybe Text)
  , _iContentLength :: !(Maybe Nat)
  , _iName          :: !(Maybe Text)
  , _iType          :: !(Maybe ItemType)
  , _iLastModified  :: !(Maybe POSIX)
  , _iContentType   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Item' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iETag' - The ETag that represents a unique instance of the item.
--
-- * 'iContentLength' - The length of the item in bytes.
--
-- * 'iName' - The name of the item.
--
-- * 'iType' - The item type (folder or object).
--
-- * 'iLastModified' - The date and time that the item was last modified.
--
-- * 'iContentType' - The content type of the item.
item
    :: Item
item =
  Item'
    { _iETag = Nothing
    , _iContentLength = Nothing
    , _iName = Nothing
    , _iType = Nothing
    , _iLastModified = Nothing
    , _iContentType = Nothing
    }


-- | The ETag that represents a unique instance of the item.
iETag :: Lens' Item (Maybe Text)
iETag = lens _iETag (\ s a -> s{_iETag = a})

-- | The length of the item in bytes.
iContentLength :: Lens' Item (Maybe Natural)
iContentLength = lens _iContentLength (\ s a -> s{_iContentLength = a}) . mapping _Nat

-- | The name of the item.
iName :: Lens' Item (Maybe Text)
iName = lens _iName (\ s a -> s{_iName = a})

-- | The item type (folder or object).
iType :: Lens' Item (Maybe ItemType)
iType = lens _iType (\ s a -> s{_iType = a})

-- | The date and time that the item was last modified.
iLastModified :: Lens' Item (Maybe UTCTime)
iLastModified = lens _iLastModified (\ s a -> s{_iLastModified = a}) . mapping _Time

-- | The content type of the item.
iContentType :: Lens' Item (Maybe Text)
iContentType = lens _iContentType (\ s a -> s{_iContentType = a})

instance FromJSON Item where
        parseJSON
          = withObject "Item"
              (\ x ->
                 Item' <$>
                   (x .:? "ETag") <*> (x .:? "ContentLength") <*>
                     (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "LastModified")
                     <*> (x .:? "ContentType"))

instance Hashable Item where

instance NFData Item where
