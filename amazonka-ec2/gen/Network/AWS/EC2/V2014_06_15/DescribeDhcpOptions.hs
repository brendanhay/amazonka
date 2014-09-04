{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your DHCP options sets. For more information about
-- DHCP options sets, see DHCP Options Sets in the Amazon Virtual Private
-- Cloud User Guide. Example 1 This example describes the specified DHCP
-- options set. https://ec2.amazonaws.com/?Action=DescribeDhcpOptions
-- &amp;DhcpOptionsId.1=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DescribeDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;dhcpOptionsSet&gt; &lt;item&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;dhcpConfigurationSet&gt; &lt;item&gt;
-- &lt;key&gt;domain-name&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;example.com&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;key&gt;domain-name-servers&lt;/key&gt;
-- &lt;valueSet&gt; &lt;item&gt; &lt;value&gt;10.2.5.1&lt;/value&gt;
-- &lt;/item&gt; &lt;/valueSet&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;key&gt;domain-name-servers&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;10.2.5.2&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;/dhcpConfigurationSet&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/dhcpOptionsSet&gt; &lt;/DescribeDhcpOptionsResponse&gt; Example 2 This
-- example uses filters to describe any DHCP options set that includes a
-- domain-name option whose value includes the string example.
-- https://ec2.amazonaws.com/?Action=DescribeDhcpOptions
-- &amp;Filter.1.Name=key &amp;Filter.1.Value.1=domain-name
-- &amp;Filter.2.Name=value &amp;Filter.2.Value.1=*example* &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeDhcpOptions
    (
    -- * Request
      DescribeDhcpOptions
    -- ** Request constructor
    , describeDhcpOptions
    -- ** Request lenses
    , ddosDhcpOptionsIds
    , ddosFilters

    -- * Response
    , DescribeDhcpOptionsResponse
    -- ** Response lenses
    , ddotDhcpOptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDhcpOptions' request.
describeDhcpOptions :: DescribeDhcpOptions
describeDhcpOptions = DescribeDhcpOptions
    { _ddosDhcpOptionsIds = mempty
    , _ddosFilters = mempty
    }
{-# INLINE describeDhcpOptions #-}

data DescribeDhcpOptions = DescribeDhcpOptions
    { _ddosDhcpOptionsIds :: [Text]
      -- ^ The IDs of one or more DHCP options sets. Default: Describes all
      -- your DHCP options sets.
    , _ddosFilters :: [Filter]
      -- ^ One or more filters. dhcp-options-id - The ID of a set of DHCP
      -- options. key - The key for one of the options (for example,
      -- domain-name). value - The value for one of the options.
      -- tag:key=value - The key/value combination of a tag assigned to
      -- the resource. tag-key - The key of a tag assigned to the
      -- resource. This filter is independent of the tag-value filter. For
      -- example, if you use both the filter "tag-key=Purpose" and the
      -- filter "tag-value=X", you get any resources assigned both the tag
      -- key Purpose (regardless of what the tag's value is), and the tag
      -- value X (regardless of what the tag's key is). If you want to
      -- list only resources where Purpose is X, see the tag:key=value
      -- filter. tag-value - The value of a tag assigned to the resource.
      -- This filter is independent of the tag-key filter.
    } deriving (Show, Generic)

-- | The IDs of one or more DHCP options sets. Default: Describes all your DHCP
-- options sets.
ddosDhcpOptionsIds :: Lens' DescribeDhcpOptions ([Text])
ddosDhcpOptionsIds f x =
    f (_ddosDhcpOptionsIds x)
        <&> \y -> x { _ddosDhcpOptionsIds = y }
{-# INLINE ddosDhcpOptionsIds #-}

-- | One or more filters. dhcp-options-id - The ID of a set of DHCP options. key
-- - The key for one of the options (for example, domain-name). value - The
-- value for one of the options. tag:key=value - The key/value combination of
-- a tag assigned to the resource. tag-key - The key of a tag assigned to the
-- resource. This filter is independent of the tag-value filter. For example,
-- if you use both the filter "tag-key=Purpose" and the filter "tag-value=X",
-- you get any resources assigned both the tag key Purpose (regardless of what
-- the tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter.
ddosFilters :: Lens' DescribeDhcpOptions ([Filter])
ddosFilters f x =
    f (_ddosFilters x)
        <&> \y -> x { _ddosFilters = y }
{-# INLINE ddosFilters #-}

instance ToQuery DescribeDhcpOptions where
    toQuery = genericQuery def

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { _ddotDhcpOptions :: [DhcpOptions]
      -- ^ Information about one or more DHCP options sets.
    } deriving (Show, Generic)

-- | Information about one or more DHCP options sets.
ddotDhcpOptions :: Lens' DescribeDhcpOptionsResponse ([DhcpOptions])
ddotDhcpOptions f x =
    f (_ddotDhcpOptions x)
        <&> \y -> x { _ddotDhcpOptions = y }
{-# INLINE ddotDhcpOptions #-}

instance FromXML DescribeDhcpOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDhcpOptions where
    type Sv DescribeDhcpOptions = EC2
    type Rs DescribeDhcpOptions = DescribeDhcpOptionsResponse

    request = post "DescribeDhcpOptions"
    response _ = xmlResponse
