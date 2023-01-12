{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OAM.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OAM.Lens
  ( -- * Operations

    -- ** CreateLink
    createLink_tags,
    createLink_labelTemplate,
    createLink_resourceTypes,
    createLink_sinkIdentifier,
    createLinkResponse_arn,
    createLinkResponse_id,
    createLinkResponse_label,
    createLinkResponse_labelTemplate,
    createLinkResponse_resourceTypes,
    createLinkResponse_sinkArn,
    createLinkResponse_tags,
    createLinkResponse_httpStatus,

    -- ** CreateSink
    createSink_tags,
    createSink_name,
    createSinkResponse_arn,
    createSinkResponse_id,
    createSinkResponse_name,
    createSinkResponse_tags,
    createSinkResponse_httpStatus,

    -- ** DeleteLink
    deleteLink_identifier,
    deleteLinkResponse_httpStatus,

    -- ** DeleteSink
    deleteSink_identifier,
    deleteSinkResponse_httpStatus,

    -- ** GetLink
    getLink_identifier,
    getLinkResponse_arn,
    getLinkResponse_id,
    getLinkResponse_label,
    getLinkResponse_labelTemplate,
    getLinkResponse_resourceTypes,
    getLinkResponse_sinkArn,
    getLinkResponse_tags,
    getLinkResponse_httpStatus,

    -- ** GetSink
    getSink_identifier,
    getSinkResponse_arn,
    getSinkResponse_id,
    getSinkResponse_name,
    getSinkResponse_tags,
    getSinkResponse_httpStatus,

    -- ** GetSinkPolicy
    getSinkPolicy_sinkIdentifier,
    getSinkPolicyResponse_policy,
    getSinkPolicyResponse_sinkArn,
    getSinkPolicyResponse_sinkId,
    getSinkPolicyResponse_httpStatus,

    -- ** ListAttachedLinks
    listAttachedLinks_maxResults,
    listAttachedLinks_nextToken,
    listAttachedLinks_sinkIdentifier,
    listAttachedLinksResponse_nextToken,
    listAttachedLinksResponse_httpStatus,
    listAttachedLinksResponse_items,

    -- ** ListLinks
    listLinks_maxResults,
    listLinks_nextToken,
    listLinksResponse_nextToken,
    listLinksResponse_httpStatus,
    listLinksResponse_items,

    -- ** ListSinks
    listSinks_maxResults,
    listSinks_nextToken,
    listSinksResponse_nextToken,
    listSinksResponse_httpStatus,
    listSinksResponse_items,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutSinkPolicy
    putSinkPolicy_sinkIdentifier,
    putSinkPolicy_policy,
    putSinkPolicyResponse_policy,
    putSinkPolicyResponse_sinkArn,
    putSinkPolicyResponse_sinkId,
    putSinkPolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLink
    updateLink_identifier,
    updateLink_resourceTypes,
    updateLinkResponse_arn,
    updateLinkResponse_id,
    updateLinkResponse_label,
    updateLinkResponse_labelTemplate,
    updateLinkResponse_resourceTypes,
    updateLinkResponse_sinkArn,
    updateLinkResponse_tags,
    updateLinkResponse_httpStatus,

    -- * Types

    -- ** ListAttachedLinksItem
    listAttachedLinksItem_label,
    listAttachedLinksItem_linkArn,
    listAttachedLinksItem_resourceTypes,

    -- ** ListLinksItem
    listLinksItem_arn,
    listLinksItem_id,
    listLinksItem_label,
    listLinksItem_resourceTypes,
    listLinksItem_sinkArn,

    -- ** ListSinksItem
    listSinksItem_arn,
    listSinksItem_id,
    listSinksItem_name,
  )
where

import Amazonka.OAM.CreateLink
import Amazonka.OAM.CreateSink
import Amazonka.OAM.DeleteLink
import Amazonka.OAM.DeleteSink
import Amazonka.OAM.GetLink
import Amazonka.OAM.GetSink
import Amazonka.OAM.GetSinkPolicy
import Amazonka.OAM.ListAttachedLinks
import Amazonka.OAM.ListLinks
import Amazonka.OAM.ListSinks
import Amazonka.OAM.ListTagsForResource
import Amazonka.OAM.PutSinkPolicy
import Amazonka.OAM.TagResource
import Amazonka.OAM.Types.ListAttachedLinksItem
import Amazonka.OAM.Types.ListLinksItem
import Amazonka.OAM.Types.ListSinksItem
import Amazonka.OAM.UntagResource
import Amazonka.OAM.UpdateLink
