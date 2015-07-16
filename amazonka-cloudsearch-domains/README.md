# Amazon CloudSearch Domain SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.0.0`


## Description

You use the AmazonCloudSearch2013 API to upload documents to a search
domain and search those documents.

The endpoints for submitting @UploadDocuments@, @Search@, and @Suggest@
requests are domain-specific. To get the endpoints for your domain, use
the Amazon CloudSearch configuration service @DescribeDomains@ action.
The domain endpoints are also displayed on the domain dashboard in the
Amazon CloudSearch console. You submit suggest requests to the search
endpoint.

For more information, see the
<http://docs.aws.amazon.com/cloudsearch/latest/developerguide Amazon CloudSearch Developer Guide>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudsearch-domains)
and the [AWS API Reference](http://docs.aws.amazon.com/cloudsearch/latest/developerguide/what-is-cloudsearch.html).


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-cloudsearch-domains` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
