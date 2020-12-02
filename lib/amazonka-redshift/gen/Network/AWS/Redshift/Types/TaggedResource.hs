{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TaggedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TaggedResource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | A tag and its associated resource.
--
--
--
-- /See:/ 'taggedResource' smart constructor.
data TaggedResource = TaggedResource'
  { _trTag :: !(Maybe Tag),
    _trResourceType :: !(Maybe Text),
    _trResourceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaggedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trTag' - The tag for the resource.
--
-- * 'trResourceType' - The type of resource with which the tag is associated. Valid resource types are:      * Cluster     * CIDR/IP     * EC2 security group     * Snapshot     * Cluster security group     * Subnet group     * HSM connection     * HSM certificate     * Parameter group For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
--
-- * 'trResourceName' - The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
taggedResource ::
  TaggedResource
taggedResource =
  TaggedResource'
    { _trTag = Nothing,
      _trResourceType = Nothing,
      _trResourceName = Nothing
    }

-- | The tag for the resource.
trTag :: Lens' TaggedResource (Maybe Tag)
trTag = lens _trTag (\s a -> s {_trTag = a})

-- | The type of resource with which the tag is associated. Valid resource types are:      * Cluster     * CIDR/IP     * EC2 security group     * Snapshot     * Cluster security group     * Subnet group     * HSM connection     * HSM certificate     * Parameter group For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
trResourceType :: Lens' TaggedResource (Maybe Text)
trResourceType = lens _trResourceType (\s a -> s {_trResourceType = a})

-- | The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
trResourceName :: Lens' TaggedResource (Maybe Text)
trResourceName = lens _trResourceName (\s a -> s {_trResourceName = a})

instance FromXML TaggedResource where
  parseXML x =
    TaggedResource'
      <$> (x .@? "Tag") <*> (x .@? "ResourceType") <*> (x .@? "ResourceName")

instance Hashable TaggedResource

instance NFData TaggedResource
