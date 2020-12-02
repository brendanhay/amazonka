{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroupsTagging.Types.Sum

-- | Details of the common errors that all actions return.
--
--
--
-- /See:/ 'failureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { _fiErrorCode    :: !(Maybe ResourceErrorCode)
  , _fiErrorMessage :: !(Maybe Text)
  , _fiStatusCode   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailureInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiErrorCode' - The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
--
-- * 'fiErrorMessage' - The message of the common error.
--
-- * 'fiStatusCode' - The HTTP status code of the common error.
failureInfo
    :: FailureInfo
failureInfo =
  FailureInfo'
    {_fiErrorCode = Nothing, _fiErrorMessage = Nothing, _fiStatusCode = Nothing}


-- | The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
fiErrorCode :: Lens' FailureInfo (Maybe ResourceErrorCode)
fiErrorCode = lens _fiErrorCode (\ s a -> s{_fiErrorCode = a})

-- | The message of the common error.
fiErrorMessage :: Lens' FailureInfo (Maybe Text)
fiErrorMessage = lens _fiErrorMessage (\ s a -> s{_fiErrorMessage = a})

-- | The HTTP status code of the common error.
fiStatusCode :: Lens' FailureInfo (Maybe Int)
fiStatusCode = lens _fiStatusCode (\ s a -> s{_fiStatusCode = a})

instance FromJSON FailureInfo where
        parseJSON
          = withObject "FailureInfo"
              (\ x ->
                 FailureInfo' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "StatusCode"))

instance Hashable FailureInfo where

instance NFData FailureInfo where

-- | A list of resource ARNs and the tags (keys and values) that are associated with each.
--
--
--
-- /See:/ 'resourceTagMapping' smart constructor.
data ResourceTagMapping = ResourceTagMapping'
  { _rtmResourceARN :: !(Maybe Text)
  , _rtmTags        :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTagMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtmResourceARN' - An array of resource ARN(s).
--
-- * 'rtmTags' - The tags that have been applied to one or more AWS resources.
resourceTagMapping
    :: ResourceTagMapping
resourceTagMapping =
  ResourceTagMapping' {_rtmResourceARN = Nothing, _rtmTags = Nothing}


-- | An array of resource ARN(s).
rtmResourceARN :: Lens' ResourceTagMapping (Maybe Text)
rtmResourceARN = lens _rtmResourceARN (\ s a -> s{_rtmResourceARN = a})

-- | The tags that have been applied to one or more AWS resources.
rtmTags :: Lens' ResourceTagMapping [Tag]
rtmTags = lens _rtmTags (\ s a -> s{_rtmTags = a}) . _Default . _Coerce

instance FromJSON ResourceTagMapping where
        parseJSON
          = withObject "ResourceTagMapping"
              (\ x ->
                 ResourceTagMapping' <$>
                   (x .:? "ResourceARN") <*> (x .:? "Tags" .!= mempty))

instance Hashable ResourceTagMapping where

instance NFData ResourceTagMapping where

-- | The metadata that you apply to AWS resources to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-basics Tag Basics> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- * 'tagValue' - The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

-- | A list of tags (keys and values) that are used to specify the associated resources.
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
  { _tfValues :: !(Maybe [Text])
  , _tfKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfValues' - The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
--
-- * 'tfKey' - One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
tagFilter
    :: TagFilter
tagFilter = TagFilter' {_tfValues = Nothing, _tfKey = Nothing}


-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\ s a -> s{_tfValues = a}) . _Default . _Coerce

-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
tfKey :: Lens' TagFilter (Maybe Text)
tfKey = lens _tfKey (\ s a -> s{_tfKey = a})

instance Hashable TagFilter where

instance NFData TagFilter where

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tfValues, ("Key" .=) <$> _tfKey])
